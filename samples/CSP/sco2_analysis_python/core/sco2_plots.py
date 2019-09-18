# -*- coding: utf-8 -*-
"""
Created on Tue Mar 13 13:03:43 2018

@author: tneises
"""

import matplotlib.pyplot as plt

import math

import pandas as pd

import numpy as np

import json

import string

import os

import matplotlib.lines as mlines

from matplotlib.ticker import MultipleLocator

from matplotlib.ticker import AutoMinorLocator

import sco2_cycle_ssc as py_sco2

def filter_dict_keys(data, keys):
    return {k:v for (k,v) in data.items() if k in keys}

def filter_dict_to_index(data, i):
    data_new = {}
    for key in data.keys():
        data_new[key] = data[key][i]
    return data_new
        
def filter_dict_column_length(data, n_len):
    data_new = {}
    for key in data.keys():
        if(len(data[key]) == n_len):
            data_new[key] = data[key]
        
    return data_new

def filter_dict_index_and_keys(data, i, keys):
    return filter_dict_to_index(filter_dict_keys(data, keys),i)

def ceil_nearest_base(x, base):
    return int(base * math.ceil(float(x)/base))
  
class C_sco2_cycle_TS_plot:

    def __init__(self, dict_cycle_data):
        self.dict_cycle_data = dict_cycle_data
        self.is_save_plot = False
        self.is_add_recup_in_out_lines = True
        self.is_annotate = True
        self.is_annotate_HTR = True
        self.is_annotate_LTR = True
        self.is_annotate_PHX = True
        self.is_add_P_const_lines = True
        self.is_add_dome = True
        self.is_add_title = True
        self.is_overwrite_title = ""
        self.file_name = ""
        self.lc = 'k'
        self.mt = 'o'
        self.markersize = 4
        
    def plot_new_figure(self):
        
        fig1, ax1 = plt.subplots(num = 1,figsize=(7.0,4.5))
    
        self.plot_from_existing_axes(ax1)
        
        plt.tight_layout(pad=0.0,h_pad=.30,rect=(0.02,0.01,0.99,0.98))
        
        if(self.is_save_plot):
            
            str_file_name = cycle_label(self.dict_cycle_data, False, True) + "__TS_plot.png"
            
            if(self.file_name != ""):
                str_file_name = self.file_name + ".png"
                                       
            fig1.savefig(str_file_name)
            
            plt.close()
        
    def plot_from_existing_axes(self, ax_in):
        
        eta_str = "Thermal Efficiency = " + '{:.1f}'.format(self.dict_cycle_data["eta_thermal_calc"]*100) + "%"
    
        plot_title = self.is_overwrite_title
    
        if(self.dict_cycle_data["cycle_config"] == 1):
            if(self.is_overwrite_title == ""):
                plot_title = "Recompression Cycle: " + eta_str
        else:
            if(self.is_overwrite_title == ""):
                plot_title = "Partial Cooling Cycle, " + eta_str
        
        self.overlay_cycle_data(ax_in)
            
        if(self.is_add_recup_in_out_lines):
            self.add_recup_in_out_lines(ax_in)
            
        if(self.is_annotate):
            self.annotate(ax_in)
            
        self.format_axes(ax_in, plot_title)
        
        return ax_in
    
    def overlay_cycle_data(self, ax_in):
        
        if(self.dict_cycle_data["cycle_config"] == 1):
            self.plot_RC_points_and_lines(ax_in)
        else:
            self.plot_PC_points_and_lines(ax_in)
        
    
    def format_axes(self, ax_in, plot_title):
    
        ax_in.autoscale()
        x_low, x_high = ax_in.get_xlim()
        
        if(self.is_add_P_const_lines):
            self.plot_constP(ax_in)      # add_Ts_constP(ax_in)
        
        if(self.is_add_dome):
            self.plot_dome(ax_in)       # add_Ts_dome(ax_in)
    
        ax_in.grid(alpha=0.5,which='major')
        ax_in.grid(alpha=0.3,which='minor')
        
        y_down, y_up = ax_in.get_ylim()
        y_min = 0
        ax_in.set_ylim(y_min, ceil_nearest_base(y_up, 100.0))
        ax_in.set_xlim(x_low)
        y_down, y_up = ax_in.get_ylim()
        major_y_ticks = np.arange(y_min,y_up+1,100)
        minor_y_ticks = np.arange(y_min,y_up+1,20)
        ax_in.set_yticks(major_y_ticks)
        ax_in.set_yticks(minor_y_ticks,minor=True)
        ax_in.set_ylabel("Temperature [C]", fontsize = 12)
        ax_in.set_xlabel("Entropy [kJ/kg-K]", fontsize = 12)
        
        if(self.is_add_title):
            ax_in.set_title(plot_title, fontsize = 14)
            
        return ax_in
    
    def plot_dome(self, ax_in):
    
        fileDir = os.path.dirname(os.path.abspath(__file__))
        
        dome_data = pd.read_csv(fileDir + "/property_data/ts_dome_data.txt")
        ax_in.plot(dome_data["s"], dome_data["T"], 'k-', lw = 1, alpha = 0.4)
    
    def annotate(self, ax_in):
    
        if(self.is_annotate_HTR):
            HTR_title = r'$\bfHigh$' + " " + r'$\bf{Temp}$' + " " + r'$\bf{Recup}$'
            q_dot_text = "\nDuty = " + '{:.1f}'.format(self.dict_cycle_data["q_dot_HTR"]) + " MWt"
            UA_text = "\nUA = " + '{:.1f}'.format(self.dict_cycle_data["HTR_UA_calculated"]) + " MW/K"
            eff_text = "\n" + r'$\epsilon$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eff_HTR"])
        
            T_HTR_LP_data = self.dict_cycle_data["T_HTR_LP_data"]
            s_HTR_LP_data = self.dict_cycle_data["s_HTR_LP_data"]
            
            n_p = len(T_HTR_LP_data)
            n_mid = (int)(n_p/2)
        
            HTR_text = HTR_title + q_dot_text + UA_text + eff_text
            
            ax_in.annotate(HTR_text, xy=(s_HTR_LP_data[n_mid],T_HTR_LP_data[n_mid]), 
                           xytext=(s_HTR_LP_data[0],T_HTR_LP_data[n_mid]), va="center",
                           arrowprops = dict(arrowstyle="->", color = 'r', ls = '--', lw = 0.6),
                           fontsize = 8,
                           bbox=dict(boxstyle="round", fc="w", pad = 0.5))
        
        if(self.is_annotate_LTR):
            LTR_title = r'$\bfLow$' + " " + r'$\bf{Temp}$' + " " + r'$\bf{Recup}$'
            q_dot_text = "\nDuty = " + '{:.1f}'.format(self.dict_cycle_data["q_dot_LTR"]) + " MWt"
            UA_text = "\nUA = " + '{:.1f}'.format(self.dict_cycle_data["LTR_UA_calculated"]) + " MW/K"
            eff_text = "\n" + r'$\epsilon$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eff_LTR"])
            
            T_LTR_LP_data = self.dict_cycle_data["T_LTR_LP_data"]
            s_LTR_LP_data = self.dict_cycle_data["s_LTR_LP_data"]
            
            n_p = len(T_LTR_LP_data)
            n_mid = (int)(n_p/2)
            
            LTR_text = LTR_title + q_dot_text + UA_text + eff_text
            
            ax_in.annotate(LTR_text, xy=(s_LTR_LP_data[n_mid],T_LTR_LP_data[n_mid]), 
                           xytext=(s_HTR_LP_data[n_mid],T_LTR_LP_data[n_mid]), va="center",
                           arrowprops = dict(arrowstyle="->", color = 'b', ls = '--', lw = 0.6),
                           fontsize = 8,
                           bbox=dict(boxstyle="round", fc="w", pad = 0.5))
        
        if(self.is_annotate_PHX):
            T_states = self.dict_cycle_data["T_state_points"]
            s_states = self.dict_cycle_data["s_state_points"]
            
            dT_PHX_approach = self.dict_cycle_data["dT_PHX_hot_approach"]
            T_PHX_in = T_states[5] + dT_PHX_approach
            s_PHX_in = s_states[5]
            
            T_PHX_out = T_states[4] + dT_PHX_approach
            s_PHX_out = s_states[4]
            
            ax_in.plot([s_PHX_in, s_PHX_out], [T_PHX_in, T_PHX_out], color = '#ff9900', ls = ":")
            
            s_PHX_avg = 0.5*(s_PHX_in + s_PHX_out)
            T_PHX_avg = 0.5*(T_PHX_in + T_PHX_out)
            
            PHX_title = r'$\bfPrimary$' + " " + r'$\bf{HX}$'
            q_dot_text = "\nDuty = " + '{:.1f}'.format(self.dict_cycle_data["q_dot_PHX"]) + " MWt"
            UA_text = "\nUA = " + '{:.1f}'.format(self.dict_cycle_data["UA_PHX"]) + " MW/K"
            eff_text = "\n" + r'$\epsilon$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eff_PHX"])    
            
            PHX_text = PHX_title + q_dot_text + UA_text + eff_text
            
            ax_in.annotate(PHX_text, xy=(s_PHX_avg, T_PHX_avg), 
                           xytext=(s_PHX_avg - 0.75,T_PHX_avg), va="center",
                           arrowprops = dict(arrowstyle="->", color = '#ff9900', ls = '--', lw = 0.6),
                           fontsize = 8,
                           bbox=dict(boxstyle="round", fc="w", pad = 0.5))
            
            return ax_in
    
    def add_recup_in_out_lines(self, ax_in):
    
        T_states = self.dict_cycle_data["T_state_points"]
        s_states = self.dict_cycle_data["s_state_points"]
        
        T_LTR_hot = [T_states[2],T_states[7]]
        s_LTR_hot = [s_states[2],s_states[7]]
        ax_in.plot(s_LTR_hot, T_LTR_hot, 'b-.', lw = 0.7, alpha = 0.9)
        
        T_LTR_cold = [T_states[1],T_states[8]]
        s_LTR_cold = [s_states[1],s_states[8]]
        ax_in.plot(s_LTR_cold, T_LTR_cold, 'b-.', lw = 0.7, alpha = 0.9)
        
        T_HTR_cold = [T_states[3],T_states[7]]
        s_HTR_cold = [s_states[3],s_states[7]]
        ax_in.plot(s_HTR_cold, T_HTR_cold, 'r-.', lw = 0.7, alpha = 0.9)
        
        T_HTR_hot = [T_states[4],T_states[6]]
        s_HTR_hot = [s_states[4],s_states[6]]
        ax_in.plot(s_HTR_hot, T_HTR_hot, 'r-.', lw = 0.7, alpha = 0.9)
        
        return ax_in
        
    def plot_hx(self, ax_in):
        
        T_LTR_HP_data = self.dict_cycle_data["T_LTR_HP_data"]
        s_LTR_HP_data = self.dict_cycle_data["s_LTR_HP_data"]
        ax_in.plot(s_LTR_HP_data, T_LTR_HP_data, self.lc)
        
        T_HTR_HP_data = self.dict_cycle_data["T_HTR_HP_data"]
        s_HTR_HP_data = self.dict_cycle_data["s_HTR_HP_data"]
        ax_in.plot(s_HTR_HP_data, T_HTR_HP_data, self.lc)
        
        T_PHX_data = self.dict_cycle_data["T_PHX_data"]
        s_PHX_data = self.dict_cycle_data["s_PHX_data"]
        ax_in.plot(s_PHX_data, T_PHX_data, self.lc)
        
        T_HTR_LP_data = self.dict_cycle_data["T_HTR_LP_data"]
        s_HTR_LP_data = self.dict_cycle_data["s_HTR_LP_data"]
        ax_in.plot(s_HTR_LP_data, T_HTR_LP_data, self.lc)
        
        T_LTR_LP_data = self.dict_cycle_data["T_LTR_LP_data"]
        s_LTR_LP_data = self.dict_cycle_data["s_LTR_LP_data"]
        ax_in.plot(s_LTR_LP_data, T_LTR_LP_data, self.lc)
    
        T_main_cooler_data = self.dict_cycle_data["T_main_cooler_data"]
        s_main_cooler_data = self.dict_cycle_data["s_main_cooler_data"]
        ax_in.plot(s_main_cooler_data, T_main_cooler_data, self.lc)
        
        T_pre_cooler_data = self.dict_cycle_data["T_pre_cooler_data"]
        s_pre_cooler_data = self.dict_cycle_data["s_pre_cooler_data"]
        ax_in.plot(s_pre_cooler_data, T_pre_cooler_data, self.lc)
        
        return ax_in
    
    def plot_RC_points_and_lines(self, ax_in):
        
        self.plot_hx(ax_in)

        T_states = self.dict_cycle_data["T_state_points"]
        s_states = self.dict_cycle_data["s_state_points"]
        
        T_mc_plot = [T_states[0],T_states[1]]
        s_mc_plot = [s_states[0],s_states[1]]
        ax_in.plot(s_mc_plot, T_mc_plot, self.lc)
        
        T_t_plot = [T_states[5],T_states[6]]
        s_t_plot = [s_states[5],s_states[6]]
        ax_in.plot(s_t_plot, T_t_plot, self.lc)
        
        ax_in.plot(s_states[0:10], T_states[0:10], self.lc + self.mt, markersize = self.markersize)
            
        f_recomp = self.dict_cycle_data["recomp_frac"]
            
        if(f_recomp > 0.01):
            T_rc_plot = [T_states[8],T_states[9]]
            s_rc_plot = [s_states[8],s_states[9]]
            ax_in.plot(s_rc_plot, T_rc_plot, self.lc)
        
        return ax_in
    
    def plot_PC_points_and_lines(self, ax_in):
        
        self.plot_hx(ax_in)

        T_states = self.dict_cycle_data["T_state_points"]
        s_states = self.dict_cycle_data["s_state_points"]
        
        T_mc_plot = [T_states[0],T_states[1]]
        s_mc_plot = [s_states[0],s_states[1]]
        ax_in.plot(s_mc_plot, T_mc_plot, self.lc)
        
        T_t_plot = [T_states[5],T_states[6]]
        s_t_plot = [s_states[5],s_states[6]]
        ax_in.plot(s_t_plot, T_t_plot, self.lc)
        
        T_pc_plot = [T_states[10],T_states[11]]
        s_pc_plot = [s_states[10],s_states[11]]
        ax_in.plot(s_pc_plot, T_pc_plot, self.lc)
        
        f_recomp = self.dict_cycle_data["recomp_frac"]
        if(f_recomp > 0.01):
            T_rc_plot = [T_states[11],T_states[9]]
            s_rc_plot = [s_states[11],s_states[9]]
            ax_in.plot(s_rc_plot, T_rc_plot, self.lc)
        
        line, = ax_in.plot(s_states, T_states, self.lc+self.mt, markersize = 4)
        
        return ax_in
    
    def plot_constP(self, ax_in):

        fileDir = os.path.dirname(os.path.abspath(__file__))
        
        P_data = pd.read_csv(fileDir + "/property_data/constantP_data.txt")
        P_vals = []
        for names in P_data.columns.values.tolist():
            if names.split("_")[1] not in P_vals:
                P_vals.append(names.split("_")[1])
        
        n_high = len(P_data["s_"+P_vals[0]].values)  - 1   
    
        n_low = round(n_high*0.98)
    
        label_pressure = True
    
        for vals in P_vals:
            ax_in.plot(P_data["s_"+vals].values, P_data["T_"+vals].values, 'k--', lw = 0.5, alpha = 0.4)
            if(label_pressure):
                ax_in.annotate("Pressure (MPa):",[P_data["s_"+vals].values[n_low], P_data["T_"+vals].values[n_low]], color = 'k', alpha = 0.4, fontsize = 8)
                label_pressure = False
            ax_in.annotate(vals,[P_data["s_"+vals].values[n_high], P_data["T_"+vals].values[n_high]], color = 'k', alpha = 0.4, fontsize = 8)

class C_sco2_cycle_PH_plot:
    
    def __init__(self, dict_cycle_data):
        self.dict_cycle_data = dict_cycle_data
        self.is_save_plot = False
        self.is_annotate = True
        self.is_annotate_MC = True
        self.is_annotate_RC = True
        self.is_annotate_PC = True
        self.is_annotate_T = True
        self.is_annotate_MC_stages = False
        self.is_annotate_RC_stages = False
        self.is_add_T_const_lines = True
        self.is_add_dome = True
        self.is_add_title = True
        self.is_overwrite_title = ""
        self.file_name = ""
        self.lc = 'k'
        self.mt = 'o'
        self.markersize = 4
        
    def plot_new_figure(self):
        
        fig1, ax1 = plt.subplots(num = 1,figsize=(7.0,4.5))

        self.plot_from_existing_axes(ax1)
    
        plt.tight_layout(pad=0.0,h_pad=.30,rect=(0.02,0.01,0.99,0.98))
        
        if(self.is_save_plot):    
        
            str_file_name = cycle_label(self.dict_cycle_data, False, True) + "__PH_plot.png"
            
            if(self.file_name != ""):
                str_file_name = self.file_name + ".png"
                                       
            fig1.savefig(str_file_name)
            
            plt.close()
            
            
    def plot_from_existing_axes(self, ax_in):        

        eta_str = "Thermal Efficiency = " + '{:.1f}'.format(self.dict_cycle_data["eta_thermal_calc"]*100) + "%"
    
        plot_title = self.is_overwrite_title
        
        if(self.dict_cycle_data["cycle_config"] == 1):
            if(self.is_overwrite_title == ""):
                plot_title = "Recompression Cycle: " + eta_str
        else:
            if(self.is_overwrite_title == ""):
                plot_title = "Partial Cooling Cycle, " + eta_str
                
        self.overlay_cycle_data(ax_in)
        
        if(self.is_annotate):
            self.annotate(ax_in)
        
        self.format_axes(ax_in, plot_title)
        
        return ax_in
    
    def overlay_cycle_data(self, ax_in):
        
        if(self.dict_cycle_data["cycle_config"] == 1):
            self.plot_RC_points_and_lines(ax_in)
        else:
            self.plot_PC_points_and_lines(ax_in)
    
    def plot_RC_points_and_lines(self, ax_in):
        
        self.plot_shared_points_and_lines(ax_in)

        P_states = self.dict_cycle_data["P_state_points"]
        h_states = self.dict_cycle_data["h_state_points"]
        
        "Main cooler"
        ax_in.plot([h_states[8],h_states[0]],[P_states[8],P_states[0]], self.lc)
    
        ax_in.plot(h_states[0:10], P_states[0:10], self.lc + self.mt, markersize = 4)
    
        f_recomp = self.dict_cycle_data["recomp_frac"]
            
        if(f_recomp > 0.01):
            P_rc_data = self.dict_cycle_data["P_rc_data"]
            h_rc_data = self.dict_cycle_data["h_rc_data"]
            ax_in.plot(h_rc_data, P_rc_data, self.lc)
        
        return ax_in
    
    def plot_PC_points_and_lines(self, ax_in):
        
        self.plot_shared_points_and_lines(ax_in)
        
        P_states = self.dict_cycle_data["P_state_points"]
        h_states = self.dict_cycle_data["h_state_points"]
        
        "Pre cooler"
        ax_in.plot([h_states[8],h_states[10]],[P_states[8],P_states[10]], self.lc)
        
        "Pre compressor"
        P_pc_data = self.dict_cycle_data["P_pc_data"]
        h_pc_data = self.dict_cycle_data["h_pc_data"]
        ax_in.plot(h_pc_data, P_pc_data, self.lc)
        
        "Main cooler"
        ax_in.plot([h_states[11],h_states[0]],[P_states[11],P_states[0]], self.lc)
    
        ax_in.plot(h_states, P_states, self.lc + self.mt, markersize = 4)
    
        f_recomp = self.dict_cycle_data["recomp_frac"]
            
        if(f_recomp > 0.01):
            P_rc_data = self.dict_cycle_data["P_rc_data"]
            h_rc_data = self.dict_cycle_data["h_rc_data"]
            ax_in.plot(h_rc_data, P_rc_data, self.lc)
        
        return ax_in
    
    def plot_shared_points_and_lines(self, ax_in):
        
        P_states = self.dict_cycle_data["P_state_points"]
        h_states = self.dict_cycle_data["h_state_points"]
        
        "Main compressor"
        P_mc_data = self.dict_cycle_data["P_mc_data"]
        h_mc_data = self.dict_cycle_data["h_mc_data"]
        ax_in.plot(h_mc_data, P_mc_data, self.lc)
        
        "LTR HP"
        ax_in.plot([h_states[1],h_states[2]],[P_states[1],P_states[2]], self.lc)
        
        "HTR HP"
        ax_in.plot([h_states[3],h_states[4]],[P_states[3],P_states[4]], self.lc)
        
        "PHX"
        ax_in.plot([h_states[4],h_states[5]],[P_states[4],P_states[5]], self.lc)
        
        "Turbine"
        P_t_data = self.dict_cycle_data["P_t_data"]
        h_t_data = self.dict_cycle_data["h_t_data"]
        ax_in.plot(h_t_data, P_t_data, self.lc)
        
        "HTR LP"
        ax_in.plot([h_states[6],h_states[7]],[P_states[6],P_states[7]], self.lc)
        
        "LTR LP"
        ax_in.plot([h_states[7],h_states[8]],[P_states[7],P_states[8]], self.lc)
        
        return ax_in
    
    def format_axes(self, ax_in, plot_title):
            
        ax_in.margins(x=0.1)
        
        ax_in.autoscale()
        y_down, y_up = ax_in.get_ylim()
        x_low, x_high = ax_in.get_xlim()
        
        if(self.is_add_T_const_lines):
            self.plot_constT(ax_in)     # add_Ph_constT(ax_in)
        if(self.is_add_dome):
            self.plot_dome(ax_in)        # add_Ph_dome(ax_in)
        
        ax_in.grid(alpha=0.5,which='major')
        ax_in.grid(alpha=0.3,which='minor')
        
        deltaP_base = 5
        ax_in.set_ylim(ceil_nearest_base(y_down - deltaP_base, deltaP_base), ceil_nearest_base(y_up, deltaP_base))
        y_down, y_up = ax_in.get_ylim()
        
        deltah_base = 100
        x_min = ceil_nearest_base(x_low - deltah_base, deltah_base)
        x_max = ceil_nearest_base(x_high, deltah_base)
        ax_in.set_xlim(x_low, x_high)
        major_x_ticks = np.arange(x_min, x_max+1,deltah_base)
        ax_in.set_xticks(major_x_ticks)
        
        ax_in.set_ylabel("Pressure [MPa]", fontsize = 12)
        ax_in.set_xlabel("Enthalpy [kJ/kg]", fontsize = 12)
        if(self.is_add_title):
            ax_in.set_title(plot_title, fontsize = 14) 
            
    def plot_constT(self, ax_in):

        fileDir = os.path.dirname(os.path.abspath(__file__))
        
        T_data = pd.read_csv(fileDir + "/property_data/constantT_data.txt")
        T_vals = []
        for names in T_data.columns.values.tolist():
            if names.split("_")[1] not in T_vals:
                T_vals.append(names.split("_")[1])
        
        i_last = len(T_data["P_"+T_vals[0]].values)  - 1
        i_ann = int(0.95 * i_last)
    
        for vals in T_vals:
            ax_in.plot(T_data["h_"+vals].values, T_data["P_"+vals].values, '--', color = 'tab:purple', lw = 0.5, alpha = 0.65)
            ax_in.annotate(vals+"C",[T_data["h_"+vals].values[i_ann], T_data["P_"+vals].values[i_ann]], color = 'tab:purple', ha="center", alpha = 0.65, fontsize = 8)
    
    def plot_dome(self, ax_in):
    
        fileDir = os.path.dirname(os.path.abspath(__file__))
        
        ph_dome_data = pd.read_csv(fileDir + "/property_data/Ph_dome_data.txt")
        ax_in.plot(ph_dome_data["h"], ph_dome_data["P"], 'k-', lw = 1, alpha = 0.4)
    
    def annotate(self, ax_in):

        m_dot_co2_full = self.dict_cycle_data["m_dot_co2_full"]
        f_recomp = self.dict_cycle_data["recomp_frac"]
        m_dot_mc = m_dot_co2_full * (1.0 - f_recomp)
        m_dot_rc = m_dot_co2_full * f_recomp
        
        mc_title = r'$\bfMain$' + " " + r'$\bfCompressor$'
        m_dot_text = "\n" + r'$\.m$' + " = " + '{:.1f}'.format(m_dot_mc) + " kg/s"
        W_dot_text = "\nPower = " + '{:.1f}'.format(self.dict_cycle_data["mc_W_dot"]) + " MW"
        isen_text = "\n" + r'$\eta_{isen}$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eta_isen_mc"])        
        
        mc_text = mc_title + m_dot_text + W_dot_text + isen_text
        if(self.is_annotate_MC_stages):
            stages_text = "\nStages = " + '{:d}'.format(int(self.dict_cycle_data["mc_n_stages"]))
            l_type, d_type = py_sco2.get_entry_data_type(self.dict_cycle_data["mc_D"])
            d_text = "\nDiameters [m] ="
            t_text = "\nTip Speed [-] ="
            if(l_type == "single"):
                d_text = d_text + " " + '{:.2f}'.format(self.dict_cycle_data["mc_D"])
                t_text = t_text + " " + '{:.2f}'.format(self.dict_cycle_data["mc_tip_ratio_des"])
            elif(l_type == "list"):
                for i_d, d_s in enumerate(self.dict_cycle_data["mc_D"]):
                    if(i_d == 0):
                        if(len(self.dict_cycle_data["mc_D"]) > 1):
                            space = "\n   "
                        else:
                            space = " "
                    else:
                        space = ", "
                    d_text = d_text + space + '{:.2f}'.format(d_s)
                    t_text = t_text + space + '{:.2f}'.format(self.dict_cycle_data["mc_tip_ratio_des"][i_d])
            mc_text = mc_text + stages_text + d_text + t_text
        
        P_states = self.dict_cycle_data["P_state_points"]
        h_states = self.dict_cycle_data["h_state_points"]
    
        P_mc_avg = 0.5*(P_states[0] + P_states[1])
        h_mc_avg = 0.5*(h_states[0] + h_states[1])
        
        if(self.is_annotate_MC):
            ax_in.annotate(mc_text, xy=(h_mc_avg,P_mc_avg), va="center", ha="center", fontsize = 8,
                           bbox=dict(boxstyle="round", fc="w", pad = 0.5))
        
        t_title = r'$\bfTurbine$'
        m_dot_text = "\n" + r'$\.m$' + " = " + '{:.1f}'.format(m_dot_co2_full) + " kg/s"
        W_dot_text = "\nPower = " + '{:.1f}'.format(self.dict_cycle_data["t_W_dot"]) + " MW"
        isen_text = "\n" + r'$\eta_{isen}$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eta_isen_t"])
    
        t_text = t_title + m_dot_text + W_dot_text + isen_text
        
        P_t_avg = 0.5*(P_states[5] + P_states[6])
        h_t_avg = 0.5*(h_states[5] + h_states[6])
        
        if(self.is_annotate_T):
            ax_in.annotate(t_text, xy=(h_t_avg,P_t_avg), va="center", ha="center", fontsize = 8,
                       bbox=dict(boxstyle="round", fc="w", pad = 0.5))
        
        is_pc = self.dict_cycle_data["cycle_config"] == 2
                          
        if(is_pc):
            pc_title = r'$\bfPre$' + " " + r'$\bfCompressor$'
            m_dot_text = "\n" + r'$\.m$' + " = " + '{:.1f}'.format(m_dot_co2_full) + " kg/s"
            W_dot_text = "\nPower = " + '{:.1f}'.format(self.dict_cycle_data["pc_W_dot"]) + " MW"
            isen_text = "\n" + r'$\eta_{isen}$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eta_isen_rc"])
            
            pc_text = pc_title + m_dot_text + W_dot_text + isen_text
            
            P_pc_avg = 0.5*(P_states[10] + P_states[11])
            h_pc_avg = 0.5*(h_states[10] + h_states[11])
            
            h_pc_text = h_states[11] + 3*(h_states[11] - h_states[10])
            
            if(self.is_annotate_PC):
                ax_in.annotate(pc_text, xy=(h_pc_avg, P_pc_avg),
                               xytext=(h_pc_text, P_states[11]), va="center",
                               arrowprops = dict(arrowstyle="->", color = 'b', ls = '--', lw = 0.6),
                               fontsize = 8, bbox=dict(boxstyle="round", fc="w", pad = 0.5))
        
        if(f_recomp > 0.01):
            
            rc_title = r'$\bfRe$' + " " + r'$\bfCompressor$'
            m_dot_text = "\n" + r'$\.m$' + " = " + '{:.1f}'.format(m_dot_rc) + " kg/s"
            W_dot_text = "\nPower = " + '{:.1f}'.format(self.dict_cycle_data["rc_W_dot"]) + " MW"
            isen_text = "\n" + r'$\eta_{isen}$' + " = " + '{:.3f}'.format(self.dict_cycle_data["eta_isen_rc"])
                        
            rc_text = rc_title + m_dot_text + W_dot_text + isen_text
            if(self.is_annotate_RC_stages):
                stages_text = "\nStages = " + '{:d}'.format(int(self.dict_cycle_data["rc_n_stages"]))
                l_type, d_type = py_sco2.get_entry_data_type(self.dict_cycle_data["rc_D"])
                d_text = "\nDiameters [m] ="
                t_text = "\nTip Speed [-] ="
                if(l_type == "single"):
                    d_text = d_text + " " + '{:.2f}'.format(self.dict_cycle_data["rc_D"])
                    t_text = t_text + " " + '{:.2f}'.format(self.dict_cycle_data["rc_tip_ratio_des"])
                elif(l_type == "list"):
                    for i_d, d_s in enumerate(self.dict_cycle_data["rc_D"]):
                        if(i_d == 0):
                            if(len(self.dict_cycle_data["rc_D"]) > 1):
                                space = "\n   "
                            else:
                                space = " "
                        else:
                            space = ", "
                        d_text = d_text + space + '{:.2f}'.format(d_s)
                        t_text = t_text + space + '{:.2f}'.format(self.dict_cycle_data["rc_tip_ratio_des"][i_d])
                rc_text = rc_text + stages_text + d_text + t_text
            
            if(is_pc):
                rc_weight = 0.75
                P_rc_avg = rc_weight*P_states[9] + (1.0-rc_weight)*P_states[11]
                h_rc_avg = rc_weight*h_states[9] + (1.0-rc_weight)*h_states[11]
                h_rc_text = h_states[9] + (h_states[9] - h_states[11])
                
            else:
                rc_weight = 0.5
                P_rc_avg = rc_weight*P_states[9] + (1.0-rc_weight)*P_states[8]
                h_rc_avg = rc_weight*h_states[9] + (1.0-rc_weight)*h_states[8]
                h_rc_text = h_states[9] + (h_states[9] - h_states[8])
    
            if(self.is_annotate_RC):
                ax_in.annotate(rc_text, xy=(h_rc_avg, P_rc_avg),
                               xytext=(h_rc_text, P_rc_avg), va="center",
                               arrowprops = dict(arrowstyle="->", color = 'b', ls = '--', lw = 0.6),
                               fontsize = 8, bbox=dict(boxstyle="round", fc="w", pad = 0.5)) 
            
        return ax_in
    

class C_sco2_TS_PH_plot:
    
    def __init__(self, dict_cycle_data):
        self.dict_cycle_data = dict_cycle_data
        
        self.c_TS_plot = C_sco2_cycle_TS_plot(self.dict_cycle_data)
        self.c_PH_plot = C_sco2_cycle_PH_plot(self.dict_cycle_data)
        
        self.is_save_plot = False
        self.is_annotate = True
        self.file_name = ""
        
    def plot_new_figure(self):
        
        self.update_subplot_class_data()
        
        fig1, a_ax = plt.subplots(nrows = 2,num = 11,figsize=(7.0,8.0))
        
        self.c_PH_plot.is_add_title = False
        self.plot_from_existing_axes(a_ax)
        
        plt.tight_layout(pad=0.0,h_pad=2.0,rect=(0.02,0.01,0.99,0.98))
        
        if(self.is_save_plot):
            
            if(self.file_name == ""):
                file_name = cycle_label(self.dict_cycle_data, False, True) + "__TS_PH_plots"
            else:
                file_name = self.file_name
    
            file_name = file_name + ".png"
            
            fig1.savefig(file_name)
            
            plt.close()
            
    def plot_from_existing_axes(self, list_ax_in):
        
        self.update_subplot_class_data()
        
        self.c_TS_plot.plot_from_existing_axes(list_ax_in[0])
                
        self.c_PH_plot.plot_from_existing_axes(list_ax_in[1])
        
    def overlay_existing_axes(self, list_ax_in):
        
        self.update_subplot_class_data()
        
        self.c_TS_plot.overlay_cycle_data(list_ax_in[0])
        
        self.c_PH_plot.overlay_cycle_data(list_ax_in[1])
        
    def update_subplot_class_data(self):
        
        self.c_TS_plot.is_annotate = self.is_annotate
        self.c_PH_plot.is_annotate = self.is_annotate
        
 
class C_sco2_TS_PH_overlay_plot:

    def __init__(self, dict_cycle_data1, dict_cycle_data2):
        self.dict_cycle_data1 = dict_cycle_data1
        self.dict_cycle_data2 = dict_cycle_data2
        
        self.is_save_plot = False
        
    def plot_new_figure(self):
         
        fig1, a_ax = plt.subplots(nrows = 2,num = 1,figsize=(7.0,8.0))

        c_plot1 = C_sco2_TS_PH_plot(self.dict_cycle_data1)
        c_plot1.is_annotate = False
        c_plot1.c_TS_plot.lc = 'k'
        c_plot1.c_TS_plot.mt = 's'
        c_plot1.c_PH_plot.lc = 'k'
        c_plot1.c_PH_plot.mt = 's'
        c_plot1.overlay_existing_axes(a_ax)

        ts_legend_lines = []
        ph_legend_lines = []
    
        ts_legend_lines.append(cycle_comp_legend_line(self.dict_cycle_data1, 'k', 's'))
        ph_legend_lines.append(cycle_comp_legend_line(self.dict_cycle_data1, 'k', 's', True))
        
        
        c_plot2 = C_sco2_TS_PH_plot(self.dict_cycle_data2)
        c_plot2.is_annotate = False
        c_plot2.c_TS_plot.lc = 'b'
        c_plot2.c_PH_plot.lc = 'b'
        c_plot2.overlay_existing_axes(a_ax)

        ts_legend_lines.append(cycle_comp_legend_line(self.dict_cycle_data2, 'b', 'o'))
        ph_legend_lines.append(cycle_comp_legend_line(self.dict_cycle_data2, 'b', 'o', True))
                  
        c_plot1.c_TS_plot.format_axes(a_ax[0], "Cycle Comparison")
        c_plot1.c_PH_plot.is_add_title = False
        c_plot1.c_PH_plot.format_axes(a_ax[1], "")
        
        a_ax[0].legend(handles=ts_legend_lines, fontsize = 8)
        a_ax[1].legend(handles=ph_legend_lines, labelspacing = 1.0, loc = "center", fontsize = 8, borderpad = 1)
        
        plt.tight_layout(pad=0.0,h_pad=2.0,rect=(0.02,0.01,0.99,0.98))
        
        if(self.is_save_plot):
            
            txt_label_1 = cycle_label(self.dict_cycle_data1, False, True)
    
            txt_label_2 = cycle_label(self.dict_cycle_data2, False, True)
            
            file_name = txt_label_1 + "__vs__" + txt_label_2 + ".png"
            
            fig1.savefig(file_name)
            
            plt.close()


def custom_auto_y_axis_scale(axis_in):
    
    y_lower, y_upper = axis_in.get_ylim()
    mult = 1
    if(y_upper < 0.1):
        mult = 100
    elif(y_upper < 2):
        mult = 100
    elif(y_upper < 10):
        mult = 10
    base = np.ceil((mult*y_upper - mult*y_lower)*0.5)
    
    y_lower_new = (1/mult)*base*np.floor(mult*y_lower*0.99/base)
    y_upper_new = (1/mult)*base*np.ceil(mult*y_upper*1.01/base)
    
    #print("base = ", base)
    #print("y_lower = ", y_lower, "y_upper = ", y_upper)
    #print("y_lower_new = ", y_lower_new, "y_upper_new = ", y_upper_new)
    
    axis_in.set_ylim(y_lower_new, y_upper_new)
        
def cycle_comp_legend_line(cycle_data, color, marker, is_multi_line = False):

    label = cycle_label(cycle_data, is_multi_line, False)
                                                          
    return mlines.Line2D([],[],color = color, marker = marker, label = label)

def cycle_label(cycle_data, is_multi_line = False, is_file_name = False):
    
    if(cycle_data["cycle_config"] == 2):
        cycle_name = r'$\bf{Partial}$' + " " + r'$\bf{Cooling}$'
        cycle_abv = "PC"
    else:
        cycle_name = r'$\bf{Recompression}$'
        cycle_abv = "RC"
    
    if(is_multi_line):
        label = cycle_name + ": " + "\n" + r'$\eta$' + " = " + '{:.1f}'.format(cycle_data["eta_thermal_calc"]*100) + "%" #,\nUA = "+ '{:.1f}'.format(cycle_data["UA_recup_total"]) + " MW/K"
    elif(not(is_file_name)):
        label = cycle_name + ": " + r'$\eta$' + " = " + '{:.1f}'.format(cycle_data["eta_thermal_calc"]*100) + "%" #, UA = "+ '{:.1f}'.format(cycle_data["UA_recup_total"]) + " MW/K"
    else:
        label = cycle_abv + "_eta_"+ '{:.1f}'.format(cycle_data["eta_thermal_calc"]*100)
        #label = cycle_abv + "_UA_"+ '{:.1f}'.format(cycle_data["UA_recup_total"]) + "_eta_"+ '{:.1f}'.format(cycle_data["eta_thermal_calc"]*100)

    return label

class C_OD_stacked_outputs_plot:
    
    def __init__(self, list_dict_results):
        
        self.list_dict_results = list_dict_results
        
        self.is_save = False
        self.file_name = ""
        self.dpi = 300
        self.file_ext = ".png"
        
        self.x_var = "T_amb"
        
        self.is_legend = True
        self.leg_var = "T_amb"
        self.list_leg_spec = ""
        
        self.add_subplot_letter = False
                
        self.is_separate_design_point = False       # Separates design point in legend
        self.is_plot_each_des_pt = True             # if False, assumes each dict dataset has same design point
        self.is_plot_des_pts = True                 # if True, plot marker for design point
        self.list_des_pts = []                      # if is_plot_des_pts is False, then this list can specify which datasets' design points are plotted. e.g. [0,3]
        
        self.is_shade_infeasible = True             # for FINAL dataset, shade all subplot where any metric in y_val exceeds its value in var_info_metrics

        self.y_vars = ["eta", "W_dot"]
        
        self.plot_colors = ['k','b','g','r','c']
        self.l_s = ['-','--','-.',':']
        self.mss = ["o","d","s",'^','p']
        self.mrk_sz = 6
        self.is_line = True                        # True: correct points with line, no markers, False: plot points with markers, no lines
        self.is_change_ls_each_plot = True          # True: every dataset cycle line style; False: change line style after all colors cycle
        
        self.is_x_label_long = True
        self.h_subplot = 3.0
        self.w_subplot = 4.0
        
        self.axis_label_fontsize = 14        #8
        self.tick_lab_fontsize = 12         #8
        self.legend_fontsize = 14           #8
        
        self.n_leg_cols = 3
        self.is_label_leg_cols = ""        # LIST
        
        self.bb_y_max_is_leg = 0.92
        self.bb_h_pad = 2
        self.bb_w_pad = 0.75
        
        self.max_rows = 3

        self.var_info_metrics = py_sco2.get_des_od_label_unit_info__combined()
        self.var_results_check = "eta_thermal_od"
        self.fig_num = 1
        
    def create_plot(self):
        
        print(self.list_dict_results[0][self.var_results_check])
        
        legend_lines = []
        legend_labels = []
        
        self.x_var_des = self.var_info_metrics[self.x_var].des_var
        self.x_var_od = self.var_info_metrics[self.x_var].od_var
                                             
        if(self.is_x_label_long):
            self.x_label = self.var_info_metrics[self.x_var].l_label
        else:
            self.x_label = self.var_info_metrics[self.x_var].s_label
        
        n_subplots = len(self.y_vars)

        n_cols = (n_subplots - 1)//self.max_rows + 1
        #print("Columns = ", n_cols)
        n_rows = int(np.ceil(n_subplots/n_cols))
        #print("Rows = ", n_rows)

        f_h = self.h_subplot * n_rows
        f_w = self.w_subplot * n_cols

        fig1, a_ax = plt.subplots(nrows = n_rows, ncols = n_cols, num = self.fig_num,figsize=(f_w,f_h))
        
        n_datasets = len(self.list_dict_results)
        
        n_leg_rows = n_datasets / self.n_leg_cols

        for i in range(n_datasets):
        
            n_od_pts_i = len(self.list_dict_results[i][self.x_var_od])
            y_feasible_flag_i = [False for i in range(n_od_pts_i)]      # This is reset every dataset...
            
            color_od_i = self.plot_colors[i%len(self.plot_colors)]
            if(self.is_change_ls_each_plot):
                ls_od_i = self.l_s[i%len(self.l_s)]
            else:
                color_iteration = i // len(self.plot_colors)
                ls_od_i = self.l_s[color_iteration%len(self.l_s)]
            mrk_i = self.mss[i%len(self.mss)]
            
            ls_des_i = color_od_i + mrk_i
        
            if(self.var_info_metrics[self.x_var].des == -999):                                                                                                                           
                x_val_des_i = self.list_dict_results[i][self.x_var_des]  # Design point x value
            else:
                x_val_des_i = self.var_info_metrics[self.x_var].des      # Design point x value
                 
            des_txt_leg_i = ""
            od_txt_leg_i = ""
                                                 
            if(self.list_leg_spec != ""):
                des_txt_leg_i = self.list_leg_spec[i]
                if(not(isinstance(self.list_leg_spec[i], list))):
                    self.is_separate_design_point = False
            else:
                if(self.var_info_metrics[self.leg_var].des == -999):
                    des_leg_val_i = self.list_dict_results[i][self.var_info_metrics[self.leg_var].des_var]
                else:
                    des_leg_val_i = self.var_info_metrics[self.leg_var].des
                
                od_leg_val_i = self.list_dict_results[i][self.var_info_metrics[self.leg_var].od_var][0]
                
                des_txt_leg_i = "Design " + self.var_info_metrics[self.leg_var].l_label + " = " + "{:.2f}".format(des_leg_val_i)
                od_txt_leg_i = self.var_info_metrics[self.leg_var].l_label + " = " + "{:.2f}".format(od_leg_val_i)
            
            if( self.is_label_leg_cols != "" and len(self.is_label_leg_cols) > 1 and i % n_leg_rows == 0 ):
                if(len(self.is_label_leg_cols) == self.n_leg_cols):
                    legend_lines.append(mlines.Line2D([],[], color = 'w'))
                    legend_labels.append(self.is_label_leg_cols[int(i // n_leg_rows)])
                else:
                    print("The number of input Legend Column Labels", len(self.is_label_leg_cols),
                          "is not equal to the number of legend columns", self.n_leg_cols)
                        
            if(self.is_separate_design_point):
                if(self.is_plot_each_des_pt or i == 0):
                        # Marker
                    legend_lines.append(mlines.Line2D([],[],color = color_od_i, marker = mrk_i,  label = des_txt_leg_i))
                    legend_labels.append(des_txt_leg_i)
                    # Line
                legend_lines.append(mlines.Line2D([],[],color = color_od_i, ls = ls_od_i, label = od_txt_leg_i))
                legend_labels.append(od_txt_leg_i)                   
            else:
                    # Marker & Line
                legend_lines.append(mlines.Line2D([],[],color = color_od_i, ls = ls_od_i, marker = mrk_i,  label = des_txt_leg_i))
                legend_labels.append(des_txt_leg_i)

            if not(self.is_line):
                ls_od_i = mrk_i
            
            for j, key in enumerate(self.y_vars):
                
                j_l_i = string.ascii_lowercase[j%26]
                
                j_col = j//n_rows
                j_row = j%n_rows

                y_od_key = self.var_info_metrics[key].od_var
                y_des_key = self.var_info_metrics[key].des_var
                y_label = self.var_info_metrics[key].s_label
                y_limit = self.var_info_metrics[key].limit_var
                
                if(self.add_subplot_letter):
                    y_label = r'$\bf{' + format(j_l_i) + ")" + '}$' + " " + y_label
                        
                if(n_cols > 1):
                    j_axis = a_ax[j_row,j_col]
                elif(n_rows > 1):
                    j_axis = a_ax[j_row]
                else:
                    j_axis = a_ax
                    
                j_axis.plot(self.list_dict_results[i][self.x_var_od], self.list_dict_results[i][y_od_key], color_od_i+ls_od_i, markersize = self.mrk_sz)
                
                if(self.is_plot_des_pts or (i in self.list_des_pts)):
                    if(self.is_plot_each_des_pt or i == 0):
                        if(self.var_info_metrics[key].des_d_type == "single"):
                            if(self.var_info_metrics[key].des_var == "none"):
                                j_axis.plot(x_val_des_i, self.var_info_metrics[key].des, ls_des_i)
                            else:
                                j_axis.plot(x_val_des_i, self.list_dict_results[i][y_des_key], ls_des_i)
                        elif(self.var_info_metrics[key].des_d_type == "list"):
                            for i_s in range(len(self.list_dict_results[i][y_des_key])):
                                j_axis.plot(x_val_des_i, self.list_dict_results[i][y_des_key][i_s], ls_des_i)
                                
                if(y_limit != ""):
                    if(isinstance(y_limit, str)):
                        y_limit_list = [self.list_dict_results[i][y_limit] for ind in range(len(self.list_dict_results[i][self.x_var_od]))]
                    else:
                        y_limit_list = [y_limit for ind in range(len(self.list_dict_results[i][self.x_var_od]))]

                    j_axis.plot(self.list_dict_results[i][self.x_var_od], y_limit_list, 'm:')
                    
                if(i == n_datasets - 1):
                    
                    j_axis.set_ylabel(y_label, fontsize = self.axis_label_fontsize)
                    
                    if(self.var_info_metrics[key].y_label_style == "sci"):
                        j_axis.ticklabel_format(style="sci", axis='y', scilimits=(0,0))
                        j_axis.yaxis.get_offset_text().set_fontsize(self.tick_lab_fontsize)
                
                    j_axis.tick_params(labelsize = self.tick_lab_fontsize)
                    
                    if( self.var_info_metrics[self.x_var].y_axis_min_max != "" ):
                        j_axis.set_xlim(self.var_info_metrics[self.x_var].y_axis_min_max[0], self.var_info_metrics[self.x_var].y_axis_min_max[1])
                        
                        if( self.var_info_metrics[self.x_var].ticks != "" ):
                            j_axis.set_xticks(self.var_info_metrics[self.x_var].ticks)
                            
                    if( self.var_info_metrics[self.x_var].minloc != "" ):
                        j_axis.xaxis.set_minor_locator(AutoMinorLocator(self.var_info_metrics[self.x_var].minloc))
                
                    j_axis.grid(which = 'both', color = 'gray', alpha = 1)
                
                    if(j_row == n_rows - 1 or j == n_subplots - 1):
                        j_axis.set_xlabel(self.x_label, fontsize = self.axis_label_fontsize)
                        
                    if(self.var_info_metrics[key].y_axis_min_max == ""):
                        custom_auto_y_axis_scale(j_axis)
                    else:
                        j_axis.set_ylim(self.var_info_metrics[key].y_axis_min_max[0], self.var_info_metrics[key].y_axis_min_max[1])
                        
                    if(self.var_info_metrics[key].minloc != ""):
                        j_axis.yaxis.set_minor_locator(AutoMinorLocator(self.var_info_metrics[key].minloc))

                        
                if(self.is_shade_infeasible):
                    if(y_limit != ""):
                        if (isinstance(y_limit, str)):
                            i_y_limit = self.list_dict_results[i][y_limit]
                        else:
                            i_y_limit = y_limit
                        for j_in, y_val_local in enumerate(self.list_dict_results[i][y_od_key]):
                            if(isinstance(y_val_local,list)):
                               for k_in, y_k_val_local in enumerate(y_val_local):
                                   if(self.var_info_metrics[key].limit_var_type == "max"):
                                       if(y_k_val_local > i_y_limit):
                                           y_feasible_flag_i[j_in] = True
                                   else:
                                       if(y_k_val_local < i_y_limit):
                                           y_feasible_flag_i[j_in] = True
                            else:
                                if(self.var_info_metrics[key].limit_var_type == "max"):
                                    if(y_val_local > i_y_limit):
                                        y_feasible_flag_i[j_in] = True
                                else:
                                    if(y_val_local < i_y_limit):
                                        y_feasible_flag_i[j_in] = True

        #fig1.legend(legend_lines, legend_labels, fontsize = self.legend_fontsize, ncol = self.n_leg_cols, 
        #     loc = "upper center", columnspacing = 0.6, bbox_to_anchor = (0.5,1.0))        
        
        if(self.is_legend):
            if( self.is_label_leg_cols != "" and len(self.is_label_leg_cols) == 1):
                ii_leg = fig1.legend(legend_lines, legend_labels, title = self.is_label_leg_cols[0], fontsize = self.legend_fontsize, ncol = self.n_leg_cols, 
                     loc = "upper center", columnspacing = 0.6, bbox_to_anchor = (0.5,1.0))
                plt.setp(ii_leg.get_title(),fontsize=self.legend_fontsize)
            else:
                fig1.legend(legend_lines, legend_labels, fontsize = self.legend_fontsize, ncol = self.n_leg_cols, 
                     loc = "upper center", columnspacing = 0.6, bbox_to_anchor = (0.5,1.0))
        
        if(self.is_legend):
        	plt.tight_layout(pad=0.0,h_pad=self.bb_h_pad, w_pad = self.bb_w_pad, rect=(0.012,0.02,0.98,self.bb_y_max_is_leg))
        else:
        	plt.tight_layout(pad=0.0,h_pad=self.bb_h_pad, w_pad = self.bb_w_pad, rect=(0.02,0.02,0.99,0.96))
        
        # Hide unused subplots
        for j in range(n_subplots, n_cols*n_rows):
            j_col = j//n_rows
            j_row = j%n_rows
            
            a_ax[j_row,j_col].set_visible(False)
        
        # Shade infeasible regions
        for j, key in enumerate(self.y_vars):
            j_col = j//n_rows
            j_row = j%n_rows
            
            if(n_cols > 1):

                if(self.is_shade_infeasible and n_datasets == 1):
                    y_lower, y_upper = a_ax[j_row,j_col].get_ylim()
                    a_ax[j_row,j_col].fill_between(self.list_dict_results[0][self.x_var_od], y_lower, y_upper, where=y_feasible_flag_i, facecolor='red', alpha=0.5)
                
            else: 

                if(self.is_shade_infeasible and n_datasets == 1):
                    y_lower, y_upper = a_ax[j_row].get_ylim()
                    a_ax[j_row].fill_between(self.list_dict_results[0][self.x_var_od], y_lower, y_upper, where=y_feasible_flag_i, facecolor='red', alpha=0.5)
            
        if(self.is_save and self.file_name != ""):    
         
            plt.savefig(self.file_name + self.file_ext, dpi = self.dpi)
        
            plt.close() 

class C_des_stacked_outputs_plot:
    
    def __init__(self, list_dict_results):
        
        self.list_dict_results = list_dict_results
        
        self.is_save = False
        self.file_name = ""
        self.dpi = 300
        self.file_ext = ".png"
        
        self.x_var = "recup_tot_UA"
        
        self.is_legend = True
        self.leg_var = "min_phx_deltaT"     # variable that legend uses to differentiate datasets. overwritten if list_leg_spec is defined
        self.list_leg_spec = ""             # list of strings for legend for each dataset
        
        self.add_subplot_letter = False

        #### component limits - Partial Cooling (lots of plots)
        self.y_vars = ["eta","cycle_cost"]
        
        self.min_var = ""             # If this is defined, plot will add a point at the x value corresponding to the min value of this variable
        
        self.plot_colors = ['k','b','g','r','c']
        self.l_s = ['-','--','-.',':']
        self.mss = ["o","d","s",'^','p']
        self.is_line_mkr = True
        self.is_change_ls_each_plot = True          # True: every dataset cycle line style; False: change line style after all colors cycle
        
        self.is_x_label_long = True
        self.h_subplot = 3.0
        self.w_subplot = 4.0
        
        self.axis_label_fontsize = 14        #8
        self.tick_lab_fontsize = 12         #8
        self.legend_fontsize = 14           #8
        
        self.n_leg_cols = 3
        self.is_label_leg_cols = ""
        
        self.bb_y_max_is_leg = 0.92
        self.bb_h_pad = 2
        self.bb_w_pad = 0.75
        
        self.max_rows = 3

        self.var_info_metrics = py_sco2.get_des_od_label_unit_info__combined()
        self.var_results_check = "eta_thermal_calc"
        self.fig_num = 1
        
    def set_var_info_metrics_to_sam_mspt(self):
        
        self.var_info_metrics = py_sco2.get_sam_mspt_sco2_label_unit_info()
        self.var_results_check = "annual_cycle_output"
        
    def set_var_inf_metrics_to_sco2_mspt_combined(self):
        
        self.var_info_metrics = py_sco2.get_des_od_mspt_label_unit_info__combined()
        
    def create_plot(self):
        
        #print(self.list_dict_results[0][self.var_results_check])
        
        legend_lines = []
        legend_labels = []
        
        self.x_var_des = self.var_info_metrics[self.x_var].des_var
        
        if(self.is_x_label_long):
        	self.x_label = self.var_info_metrics[self.x_var].l_label
        else:
        	self.x_label = self.var_info_metrics[self.x_var].s_label
                                             
        n_subplots = len(self.y_vars)
        
        n_cols = (n_subplots - 1)//self.max_rows + 1
        #print("Columns = ", n_cols)
        n_rows = int(np.ceil(n_subplots/n_cols))
        #print("Rows = ", n_rows)
        
        f_h = self.h_subplot * n_rows
        f_w = self.w_subplot * n_cols
        
        fig1, a_ax = plt.subplots(nrows = n_rows, ncols = n_cols, num = self.fig_num,figsize=(f_w,f_h))
        
        n_datasets = len(self.list_dict_results)
        
        n_leg_rows = n_datasets / self.n_leg_cols
        
        for i in range(n_datasets):

            if(self.min_var != ""):
                i_min_var = self.list_dict_results[i][self.min_var].index(min(self.list_dict_results[i][self.min_var]))
            
            color_i = self.plot_colors[i%len(self.plot_colors)]
            if(self.is_change_ls_each_plot):
                ls_i = self.l_s[i%len(self.l_s)]
            else:
                color_iteration = i // len(self.plot_colors)
                ls_i = self.l_s[color_iteration%len(self.l_s)]
            mrk_i = self.mss[i%len(self.mss)]
            clr_mrk_i = color_i + mrk_i
            
            if(self.is_line_mkr):
                plt_style_i = ls_i + clr_mrk_i
            else:
                plt_style_i = color_i + ls_i
            
            i_txt_leg = ""
            
            if(self.list_leg_spec != ""):
                if(i < len(self.list_leg_spec)):
                    i_txt_leg = self.list_leg_spec[i]
            else:
                des_leg_val = self.list_dict_results[i][self.var_info_metrics[self.leg_var].des_var][0]
                i_txt_leg = self.var_info_metrics[self.leg_var].l_label + " = " + "{:2f}".format(des_leg_val)
            
            if( self.is_label_leg_cols != "" and i % n_leg_rows == 0 ):
                if(len(self.is_label_leg_cols) == self.n_leg_cols):
                    legend_lines.append(mlines.Line2D([],[], color = 'w'))
                    legend_labels.append(self.is_label_leg_cols[int(i // n_leg_rows)])                
                #else:
                #    print("The number of input Legend Column Labels", len(self.is_label_leg_cols),
        		#		  "is not equal to the number of legend columns", self.n_leg_cols)
                        
            if(self.is_line_mkr):
                legend_lines.append(mlines.Line2D([],[],color = color_i, ls = ls_i, marker = mrk_i,  label = i_txt_leg))
            else:
                legend_lines.append(mlines.Line2D([],[],color = color_i, ls = ls_i, label = i_txt_leg))
            legend_labels.append(i_txt_leg)
            
            for j, key in enumerate(self.y_vars):
                
                j_l_i = string.ascii_lowercase[j]
                
                j_col = j//n_rows
                j_row = j%n_rows
                
                y_des_key = self.var_info_metrics[key].des_var
                y_label = self.var_info_metrics[key].s_label
                                               
                if(self.add_subplot_letter):
                    y_label = r'$\bf{' + format(j_l_i) + ")" + '}$' + " " + y_label
                        
                if(n_cols > 1):
                    j_axis = a_ax[j_row,j_col]
                elif(n_rows > 1):
                    j_axis = a_ax[j_row]
                else:
                    j_axis = a_ax
                    
                j_axis.plot(self.list_dict_results[i][self.x_var_des], self.list_dict_results[i][y_des_key], plt_style_i)
                
                if(self.min_var != ""):
                    j_axis.plot(self.list_dict_results[i][self.x_var_des][i_min_var], self.list_dict_results[i][y_des_key][i_min_var], 
                                clr_mrk_i, markersize=self.tick_lab_fontsize*0.65)
                
                if(i == n_datasets - 1):
                    j_axis.set_ylabel(y_label, fontsize = self.axis_label_fontsize)
                    
                    if(self.var_info_metrics[key].y_label_style == "sci"):
                        j_axis.ticklabel_format(style="sci", axis='y', scilimits=(0,0))
                        j_axis.yaxis.get_offset_text().set_fontsize(self.tick_lab_fontsize)
                    
                    j_axis.tick_params(labelsize = self.tick_lab_fontsize)
                    
                    if( self.var_info_metrics[self.x_var].y_axis_min_max != "" ):
                        j_axis.set_xlim(self.var_info_metrics[self.x_var].y_axis_min_max[0], self.var_info_metrics[self.x_var].y_axis_min_max[1])
                        
                        if( self.var_info_metrics[self.x_var].ticks != "" ):
                            j_axis.set_xticks(self.var_info_metrics[self.x_var].ticks)
                            
                        if( self.var_info_metrics[self.x_var].minloc != "" ):
                            j_axis.xaxis.set_minor_locator(AutoMinorLocator(self.var_info_metrics[self.x_var].minloc)) #set_minor_locator(MultipleLocator(self.var_info_metrics[self.x_var].minloc))
                    
                    j_axis.grid(which = 'both', color = 'gray', alpha = 0.75)
                    
                    if(j_row == n_rows - 1 or j == n_subplots - 1):
                        j_axis.set_xlabel(self.x_label, fontsize = self.axis_label_fontsize)
                        
                        
                    if(self.var_info_metrics[key].y_axis_min_max == ""):
                        custom_auto_y_axis_scale(j_axis)
                    else:
                        j_axis.set_ylim(self.var_info_metrics[key].y_axis_min_max[0], self.var_info_metrics[key].y_axis_min_max[1])
                        
                    if(self.var_info_metrics[key].minloc != ""):
                        j_axis.yaxis.set_minor_locator(AutoMinorLocator(self.var_info_metrics[key].minloc))

        if(self.is_legend):
            if( self.is_label_leg_cols != "" and len(self.is_label_leg_cols) == 1):
                ii_leg = fig1.legend(legend_lines, legend_labels, title = self.is_label_leg_cols[0], fontsize = self.legend_fontsize, ncol = self.n_leg_cols, 
                     loc = "upper center", columnspacing = 0.6, bbox_to_anchor = (0.5,1.0))
                plt.setp(ii_leg.get_title(),fontsize=self.legend_fontsize)
            else:
                fig1.legend(legend_lines, legend_labels, fontsize = self.legend_fontsize, ncol = self.n_leg_cols, 
                     loc = "upper center", columnspacing = 0.6, bbox_to_anchor = (0.5,1.0))
        
        if(self.is_legend):
        	plt.tight_layout(pad=0.0,h_pad=self.bb_h_pad, w_pad = self.bb_w_pad, rect=(0.012,0.02,0.98,self.bb_y_max_is_leg))
        else:
        	plt.tight_layout(pad=0.0,h_pad=self.bb_h_pad, w_pad = self.bb_w_pad, rect=(0.02,0.02,0.99,0.96))
        
        # Hide unused subplots
        for j in range(n_subplots, n_cols*n_rows):
        	j_col = j//n_rows
        	j_row = j%n_rows
        	
        	a_ax[j_row,j_col].set_visible(False)

        if(self.is_save and self.file_name != ""):    
         
        	plt.savefig(self.file_name + self.file_ext, dpi = self.dpi)
        
        	plt.close() 
                
        return;

class C_stacked_cycle_outputs_comp_plot:

    def __init__(self, cycle_config, data_1, data_2, x_var, x_label, y_var_dict_ordered, 
                 plt_title, save_title,
                 data_1_desc = "", data_2_desc = ""):
        self.cycle_config = cycle_config
        self.data_1 = data_1
        self.data_1_desc = data_1_desc
        self.data_2 = data_2
        self.data_2_desc = data_2_desc
        self.x_var = x_var
        self.x_label = x_label
        self.y_var_dict_ordered = y_var_dict_ordered
                
        if(self.data_1_desc == ""):
            self.data_1_desc = "data_set_1"
            
        if(self.data_2_desc == ""):
            self.data_2_desc = "data_set_2"
            
        self.plt_title = plt_title
        
        self.save_title = save_title

    def create_stacked_cycle_output_comp(self):

        y_keys = list(self.y_var_dict_ordered.keys())
        print("Ordered y-vars to plot = ", y_keys)
  
        n_subplots = len(y_keys)
        
        h = 2.0*n_subplots
        fig1, a_ax = plt.subplots(nrows = n_subplots,num = 1,figsize=(7.0,h))
        
        for j in range(n_subplots):

            y_key = y_keys[j]
            y_label = self.y_var_dict_ordered[y_key]
            
            a_ax[j].plot(self.data_1[self.x_var], self.data_1[y_key], label = self.data_1_desc)
            a_ax[j].plot(self.data_2[self.x_var], self.data_2[y_key], label = self.data_2_desc)
            a_ax[j].set_ylabel(y_label)
            
            a_ax[j].grid(which = 'both', color = 'gray', alpha = 1)
            
            x = 0.9*a_ax[j].get_xlim()[1] + 0.1*a_ax[j].get_xlim()[0]
            y = 0.5*a_ax[j].get_ylim()[0] + 0.5*a_ax[j].get_ylim()[1]
            letter = string.ascii_lowercase[j]
            
            a_ax[j].text(x, y, letter, bbox = dict(facecolor = 'w', edgecolor = 'k'))
    
            if( j == 0 ):
                a_ax[j].set_title(self.plt_title)
                a_ax[j].legend(loc = 'center', bbox_to_anchor=(0.5,1.35),ncol=2)
                #mlines.Line2D([],[],color = color, marker = marker, label = label)
    
            if( j == n_subplots - 1 ):
                
                a_ax[j].set_xlabel(self.x_label)
        
        #plt.tight_layout(pad=0.0,h_pad=.30,rect=(0.02,0.01,0.99,0.98))
        plt.tight_layout(pad=0.0,h_pad=2.0,rect=(0.02,0.01,0.99,0.965))
        
        plt.savefig(self.save_title + ".png")
        
        plt.close()        
        
class C_UA_par__stacked_outputs_comp_plot(C_stacked_cycle_outputs_comp_plot):
    
    def __init__(self, cycle_config, data_1, data_2, data_desc_1 = "", data_desc_2  = ""):
        
        x_var = "UA_recup_tot_des"
        x_label = "Total recuperator conductance [kW/K]"
        y_var_dict_ordered = {"eta_thermal_calc" : "Thermal\nEfficiency [-]",
                                   "deltaT_HTF_PHX" : "PHX Temp\nDifference [C]",
                                   "P_comp_out" : "Comp Outlet\nPressure [MPa]",
                                   "P_cooler_in" : "Turbine Outlet\nPressure [MPa]",
                                   "P_comp_in" : "Comp Inlet\nPressure [MPa]",
                                   "recomp_frac" : "Recompression\nFraction [-]"}
        
        if(cycle_config == "partialcooling"):
            cycle_name = "Partial cooling cycle"
        elif(cycle_config == "recompression"):
            cycle_name = "Recompression cycle"
        else:
            cycle_name = "Cycle"
        
        plt_title = cycle_name + " solved metrics vs recuperator conductance"
        
        save_title = cycle_config + "_vs_" + x_var + "__" + data_desc_1 + "__comp__" + data_desc_2
        
        super().__init__(cycle_config, data_1, data_2, x_var, x_label, y_var_dict_ordered, 
             plt_title, save_title,
             data_desc_1, data_desc_2)
    
class C_UA_par__stacked_outputs_comp_plot_from_json(C_UA_par__stacked_outputs_comp_plot):

    def __init__(self, cycle_config, file_end_1, file_end_2, file_1_desc = "", file_2_desc = ""):
        
        data_1 = json.load(open(cycle_config + "_" + file_end_1 + ".txt"))
        if(file_1_desc == ""):
            file_1_desc = file_end_1
            
        data_2 = json.load(open(cycle_config + "_" + file_end_2 + ".txt"))
        if(file_2_desc == ""):
            file_2_desc = file_end_2
        
        super().__init__(cycle_config, data_1, data_2, file_1_desc, file_2_desc)


def plot_eta_vs_deltaT__constant_UA__multi_configs(list_des_results, UA_fixed):
    
    plt.figure(num = 1,figsize=(7.0,3.5))
    
    plot_colors = ['k','b','g']
    
    lss = ["-","--"]
    
    mss = ["o","d","s"]
    
    for i_cycle, val_unused in enumerate(list_des_results):

        index_UA_plot = []
        
        ls_i = plot_colors[i_cycle%len(plot_colors)] + lss[i_cycle%len(lss)] + mss[i_cycle%len(mss)]
        
        cycle_config_code = list_des_results[i_cycle]["cycle_config"][0]
        if(cycle_config_code == 1):
            cycle_config = "recompression"
        elif(cycle_config_code == 2):
            cycle_config = "partialcooling"
        else:
            cycle_config = "cycle"
        print("i_cycle = ", i_cycle, " cycle config = ", cycle_config)
        
        max_eta = max(list_des_results[i_cycle]["eta_thermal_calc"])
        
        for i, val in enumerate(list_des_results[i_cycle]["UA_recup_tot_des"]):
            if val == UA_fixed:
                if (list_des_results[i_cycle]["eta_thermal_calc"][i] > 0.8*max_eta):
                    index_UA_plot.append(i)
                
        plot_data_UA = dict((key, [val[i] for i in index_UA_plot]) for key, val in list_des_results[i_cycle].items())
        
        plt.plot(plot_data_UA["deltaT_HTF_PHX"], plot_data_UA["eta_thermal_calc"], ls_i,label = cycle_config)

        print(plot_data_UA["UA_HTR"])
        print(plot_data_UA["UA_LTR"])
        for abc in range(len(plot_data_UA["UA_HTR"])-1):
            print(plot_data_UA["UA_HTR"][abc]/(plot_data_UA["UA_LTR"][abc]+plot_data_UA["UA_HTR"][abc]))
    
    plt.xlabel("PHX Temperature Difference [C]")
    
    plt.legend(numpoints = 1, markerscale = 0.7)
    
    plt.ylabel("Thermal Efficiency [-]") 
    plt.ylim(ymax = 0.5, ymin = 0.4)
    plt.yticks(np.arange(0.4, 0.501, 0.01))
    
    plt.grid(which = 'both', color = 'gray', alpha = 1) 
    plt.tight_layout(rect=(0.01,0.01,1.0,1.0))
    plt.savefig("combined_eta_vs_deltaT.png")
    plt.close()


def plot_eta_vs_UA__add_UA_saturation_point__multi_configs(list_des_results):
    
    plt.figure(num = 1,figsize=(7.0,3.5))
    
    fs_s = 10;
    
    lss = ["-","--"]
    
    UA_max = 0.0
    
    for i_cycle, val_unused in enumerate(list_des_results):
        
        ls_i = lss[i_cycle%2]
    
        cycle_config_code = list_des_results[i_cycle]["cycle_config"][0]
        if(cycle_config_code == 1):
            cycle_config = "recompression"
        elif(cycle_config_code == 2):
            cycle_config = "partialcooling"
        else:
            cycle_config = "cycle"
        print("i_cycle = ", i_cycle, " cycle config = ", cycle_config)
    
        plt.plot(list_des_results[i_cycle]["UA_recup_tot_des"], list_des_results[i_cycle]["eta_thermal_calc"], ls_i, label = cycle_config)
        
        #max_eta = max(list_des_results[i_cycle]["eta_thermal_calc"])
        
        list_a, list_b = calculate_UA_saturated([list_des_results[i_cycle]], 0.0025/2, 0.05)
        list_des_results[i_cycle]["UA_practical"] = list_a[0]
        list_des_results[i_cycle]["eta_UA_practical"]= list_b[0]
        #list_des_results[i_cycle]["UA_practical"][0], list_des_results[i_cycle]["eta_UA_practical"][0] = calculate_UA_saturated(list_des_results[i_cycle], 0.0025/2, 0.05)
        
        # This tries to calculate the point where adding recuperator UA is "not worth it"
#        for i, val_unused_2 in enumerate(list_des_results[i_cycle]["min_phx_deltaT"]):
#            
#            if(i < len(list_des_results[i_cycle]["min_phx_deltaT"]) - 1):
#                
#                # Relative increase in UA between items
#                UA_frac = list_des_results[i_cycle]["UA_recup_tot_des"][i+1] / list_des_results[i_cycle]["UA_recup_tot_des"][i]
#                # Absolute change in efficiency between items
#                eta_increase = list_des_results[i_cycle]["eta_thermal_calc"][i+1] - list_des_results[i_cycle]["eta_thermal_calc"][i]
#                
#                # If the quotient of is equivalent to less than 0.0025 efficiency points per doubling the UA, then it's "saturated"
#                # Could also compare recuperator solution (e.g. is effectiveness == max or is UA_allocated > UA_solved)
#                if( eta_increase / UA_frac < 0.0025 /2 and list_des_results[i_cycle]["eta_thermal_calc"][i+1] > max_eta - 0.05):
#                    print(list_des_results[i_cycle]["UA_recup_tot_des"][i+1], list_des_results[i_cycle]["eta_thermal_calc"][i+1], max_eta)
#                    list_des_results[i_cycle]["UA_practical"] = list_des_results[i_cycle]["UA_recup_tot_des"][i+1]
#                    list_des_results[i_cycle]["eta_UA_practical"] = list_des_results[i_cycle]["eta_thermal_calc"][i+1]
#                    break;
#                    
#            else:
#                print ("did not find break point")
            
        # Overlay this practical maximum on eta-UA plot
        plt.plot(list_des_results[i_cycle]["UA_practical"], list_des_results[i_cycle]["eta_UA_practical"],'o')
        
        UA_max = max(UA_max, list_des_results[i_cycle]["UA_practical"])
        
    plt.ylabel("Thermal Efficiency [-]")
    plt.xlabel("Recuperator Conductance [kW/K]")
    plt.grid(which = 'both', color = 'gray', alpha = 0.75)
    plt.tight_layout(rect=(0.01,0.01,1.0,1.0))
    plt.legend(title = "Cycle Configuration", fontsize=fs_s, loc = 'lower right', labelspacing=0.25,numpoints=1)       
    plt.savefig("combined_eta_vs_UA.png")
    plt.close()
    
    #return UA_max

def calculate_UA_saturated(list_des_results, delta_eta_abs_over_UA_frac_rel_min, eta_max_diff_floor):

    # This tries to calculate the point where adding recuperator UA is "not worth it"
    UA_max = [0]
    eta_at_UA_max = [0]

    for i_cycle in range(len(list_des_results)):        

        max_eta = max(list_des_results[i_cycle]["eta_thermal_calc"])
        
        for i, val_unused_2 in enumerate(list_des_results[i_cycle]["min_phx_deltaT"]):
            
            if(i < len(list_des_results[i_cycle]["min_phx_deltaT"]) - 1):
                
                # Relative increase in UA between items
                UA_frac = list_des_results[i_cycle]["UA_recup_tot_des"][i+1] / list_des_results[i_cycle]["UA_recup_tot_des"][i]
                # Absolute change in efficiency between items
                eta_increase = list_des_results[i_cycle]["eta_thermal_calc"][i+1] - list_des_results[i_cycle]["eta_thermal_calc"][i]
                
                # If the quotient of is equivalent to less than 0.0025 efficiency points per doubling the UA, then it's "saturated"
                # Could also compare recuperator solution (e.g. is effectiveness == max or is UA_allocated > UA_solved)
                #if( eta_increase / UA_frac < 0.0025 /2 and list_des_results[i_cycle]["eta_thermal_calc"][i+1] > max_eta - 0.05)
                if( eta_increase / UA_frac < delta_eta_abs_over_UA_frac_rel_min and list_des_results[i_cycle]["eta_thermal_calc"][i+1] > max_eta - eta_max_diff_floor):
                    print(list_des_results[i_cycle]["UA_recup_tot_des"][i+1], list_des_results[i_cycle]["eta_thermal_calc"][i+1], max_eta)
                    UA_practical = list_des_results[i_cycle]["UA_recup_tot_des"][i+1]
                    eta_practical = list_des_results[i_cycle]["eta_thermal_calc"][i+1]
                    break;
                    
            else:
                UA_practical = list_des_results[i_cycle]["UA_recup_tot_des"][i]
                eta_practical = list_des_results[i_cycle]["eta_thermal_calc"][i]
                
        if(i_cycle==0):
            UA_max[0] = UA_practical
            eta_at_UA_max[0] = eta_practical
        else:
            UA_max.append(UA_practical)
            eta_at_UA_max.append(eta_practical)
    
    return UA_max, eta_at_UA_max

def plot_eta_vs_UA__deltaT_levels__two_config(list_des_results):
    
    plot_ls = ["-","--"]
    color_list = ['k','b','r','g','y','m','c','k','b','r','g']
    
    color_list = ['C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9']
    
    plot_keys = ["min_phx_deltaT", "UA_recup_tot_des", "eta_thermal_calc", "deltaT_HTF_PHX"]
    
    legend_lines = []
    
    if True:
        x_var = "UA_recup_tot_des"
        x_label = "Total Recuperator Conductance [kW/K]"
        y_var = "eta_thermal_calc"
        y_label = "Thermal Efficiency [-]"
        overlay_key = "min_phx_deltaT"
        overlay_label = " [C]"
        overlay_title = "Minimum PHX\nTemp Difference"
        save_title = "_eta_vs_UA_PHX_dT_par.png"
        
    elif False:
        x_var = "deltaT_HTF_PHX"
        x_label = "PHX Temperature Difference [C]"
        y_var = "eta_thermal_calc"
        y_label = "Thermal Efficiency [-]"
        overlay_key = "min_phx_deltaT"
        overlay_label = " [C]"
        overlay_title = "Minimum PHX\nTemp Difference"
        save_title = "_eta_vs_real_phx_dT.png"
        
    elif True:
        x_var = "deltaT_HTF_PHX"
        x_label = "PHX Temperature Difference [C]"
        y_var = "eta_thermal_calc"
        y_label = "Thermal Efficiency [-]"
        overlay_key = "UA_recup_tot_des"
        overlay_label = " [kW/K]"
        overlay_title = "Total Recup\n Conductance"
        save_title = "_eta_vs_real_phx_dT_UA.png"
    
    plt.figure(num = 1,figsize=(7.0,3.5))
    
    style_key = {}
    
    n_cycles = len(list_des_results)
    
    #for i_cycle, cycle_config in enumerate(cycle_configs):
    for i_cycle, val_unused in enumerate(list_des_results):
    
        cycle_config_code = list_des_results[i_cycle]["cycle_config"][0]
        if(cycle_config_code == 1):
            cycle_config = "recompression"
        elif(cycle_config_code == 2):
            cycle_config = "partialcooling"
        else:
            cycle_config = "cycle"
        print("i_cycle = ", i_cycle, " cycle config = ", cycle_config)
        
        legend_lines.append(mlines.Line2D([],[],color='k', ls = plot_ls[i_cycle], label = cycle_config))
        
        # Load results of min_deltaT -> UA parametric
        #data = json.load(open(cycle_config + "_dT_UA_par_sweep_q2_baseline.txt"))
        data = list_des_results[i_cycle]
        
        # Filter dictionary to contain only keys that we want for plotting
        plot_data_filtered = {k:v for (k,v) in data.items() if k in plot_keys}
        
        # Sort remaining data by outer next variable: min_phx_deltaT
        mdT = data[overlay_key]
        
        unique_mdTs = sorted(list(set(mdT)), reverse=False)
        
        plot_data = {}
        
        for dT in unique_mdTs:
    
            dT_key = str(dT)
            plot_data[dT_key] = {}
            
            plot_data[dT_key]["dT"] = dT
            
            plot_data_sorted_ind_mdTs = []
            
            for i, val in enumerate(mdT):
                if val == dT:
                    plot_data_sorted_ind_mdTs.append(i)
            
            plot_data[dT_key] = dict((key, [val[i] for i in plot_data_sorted_ind_mdTs]) for key, val in plot_data_filtered.items())
    
        if(i_cycle == 0):
            style_key = plot_data
    
        fs_s = 10;
        
        for i,key in enumerate(plot_data):
            #print(plot_data[key][x_var])
            
            if(i_cycle == 0):
                co_i = i
            else:
                co_i = i
                for k,k_key in enumerate(style_key):
                    if(k_key == key):
                        co_i = k
                        
            if(i_cycle == 0):
                plt.plot(plot_data[key][x_var],plot_data[key][y_var], plot_ls[i_cycle], color = color_list[co_i], markersize = 5, lw = 2, label = key + overlay_label)
            else:
                plt.plot(plot_data[key][x_var],plot_data[key][y_var], plot_ls[i_cycle], color = color_list[co_i], markersize = 5, lw = 2)
           
    legend1 = plt.legend(handles=legend_lines, fontsize = 8, ncol = 2, loc = "center", bbox_to_anchor = (0.5,1.05))
    plt.legend(title = overlay_title, fontsize=fs_s, loc = 'upper left', bbox_to_anchor = (1.0,1.0), labelspacing=0.25,numpoints=1)
    
    plt.gca().add_artist(legend1)
    
    plt.tight_layout(rect=(0.02,0.02,0.8,0.96))
    
    plt.ylim(ymax = 0.5, ymin = 0.3)
    plt.yticks(np.arange(0.3, 0.501, 0.02))
    plt.ylabel(y_label)
    plt.grid(which = 'both', color = 'gray', alpha = 0.5) 
    
    plt.xlim(xmax = 50000)
    plt.xlabel(x_label)   
    
    if(n_cycles == 1):
        plt.savefig(cycle_config + save_title)
    else:
        plt.savefig("overlay_cycles" + save_title)
        
    plt.close()

def plot_udpc_results(udpc_data, n_T_htf, n_T_amb, n_m_dot_htf):

    n_levels = 3

    w_pad = 3

    # Add normalized efficiency column
    for row in udpc_data:
        row.append(row[3] / row[4])

    fig1, a_ax = plt.subplots(nrows=3, ncols=2, num=1, figsize=(7, 10))

    mi = [[0, 0, 3, "Normalized Power"]]
    mi.append([1, 0, 10, "Normalized Efficiency"])
    mi.append([0, 1, 5, "Normalized Cooling Power"])
    mi.append(([1, 1, 7, "Normalized PHX deltaT"]))
    mi.append([2, 0, 8, "Normalized PHX Inlet Pressure"])
    mi.append([2, 1, 9, "Normalized PHX CO2 Mass Flow"])

    # T_htf parametric values, 3 m_dot levels, design ambient temperature
    for j in range(0, len(mi)):
        j_ax = a_ax[mi[j][0], mi[j][1]]
        for i in range(0, n_levels):
            row_start = i * n_T_htf
            row_end = i * n_T_htf + n_T_htf
            if( j == 0 ):
                j_ax.plot([k[0] for k in udpc_data[row_start:row_end]],
                      [k[mi[j][2]] for k in udpc_data[row_start:row_end]],
                        label = "m_dot_ND = " + str(udpc_data[row_start][1]))
            else:
                j_ax.plot([k[0] for k in udpc_data[row_start:row_end]],
                          [k[mi[j][2]] for k in udpc_data[row_start:row_end]])
        j_ax.set_xlabel("HTF Hot Temperature [C]")
        j_ax.set_ylabel(mi[j][3])
        j_ax.grid(which='both', color='gray', alpha=1)

    fig1.legend(ncol=n_levels, loc="upper center", columnspacing=0.6, bbox_to_anchor=(0.5, 1.0))
    plt.tight_layout(pad=0.0, h_pad=1, w_pad=w_pad, rect=(0.012, 0.02, 0.98, 0.94))
    plt.savefig("udpc_T_HTF.png")
    plt.close()

    fig1, a_ax = plt.subplots(nrows=3, ncols=2, num=1, figsize=(7, 10))

    # T_amb parametric values, 3 T_HTF_levels, design m_dot
    for j in range(0, len(mi)):
        j_ax = a_ax[mi[j][0], mi[j][1]]
        for i in range(0, n_levels):
            row_start = 3 * n_T_htf + i * n_T_amb
            row_end = row_start + n_T_amb
            if( j == 0 ):
                j_ax.plot([k[2] for k in udpc_data[row_start:row_end]],
                      [k[mi[j][2]] for k in udpc_data[row_start:row_end]],
                          label = "T_HTF = " + str(udpc_data[row_start][0]))
            else:
                j_ax.plot([k[2] for k in udpc_data[row_start:row_end]],
                          [k[mi[j][2]] for k in udpc_data[row_start:row_end]])
        j_ax.set_xlabel("Ambient Temperature [C]")
        j_ax.set_ylabel(mi[j][3])
        j_ax.grid(which='both', color='gray', alpha=1)

    fig1.legend(ncol=n_levels, loc="upper center", columnspacing=0.6, bbox_to_anchor=(0.5, 1.0))
    plt.tight_layout(pad=0.0, h_pad=1, w_pad=w_pad, rect=(0.012, 0.02, 0.98, 0.94))
    plt.savefig("udpc_T_amb.png")
    plt.close()

    fig1, a_ax = plt.subplots(nrows=3, ncols=2, num=1, figsize=(7, 10))

    # m_dot parametric values, 3 T_amb levels, design T_htf_hot
    for j in range(0, len(mi)):
        j_ax = a_ax[mi[j][0], mi[j][1]]
        for i in range(0, n_levels):
            row_start = 3 * n_T_htf + 3 * n_T_amb + i * n_m_dot_htf
            row_end = row_start + n_m_dot_htf
            if( j == 0 ):
                j_ax.plot([k[1] for k in udpc_data[row_start:row_end]],
                      [k[mi[j][2]] for k in udpc_data[row_start:row_end]],
                      label = "T_amb = " + str(udpc_data[row_start][2]))
            else:
                j_ax.plot([k[1] for k in udpc_data[row_start:row_end]],
                      [k[mi[j][2]] for k in udpc_data[row_start:row_end]])
                
        j_ax.set_xlabel("Normalized Mass Flow")
        j_ax.set_ylabel(mi[j][3])
        j_ax.grid(which='both', color='gray', alpha=1)

    fig1.legend(ncol = n_levels, loc = "upper center", columnspacing = 0.6, bbox_to_anchor = (0.5,1.0))
    plt.tight_layout(pad=0.0, h_pad=1, w_pad=w_pad, rect=(0.012, 0.02, 0.98, 0.94))
    plt.savefig("udpc_m_dot_htf.png")
    plt.close()

        
       
        
        
        
        
        
        
        
        
        
    