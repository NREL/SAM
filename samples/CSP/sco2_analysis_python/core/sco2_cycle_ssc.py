# -*- coding: utf-8 -*-
"""
Created on Tue Mar 13 13:52:53 2018

@author: tneises
"""

import csv

import time

import json

import math

import numpy as np

import pandas as pd

import sco2_plots as cy_plt

import ssc_inout_v2 as ssc_sim

class C_des_od_label_unit_info:
    
    def __init__(self, des_var, od_var, s_label, l_label, units):
        self.des_var = des_var
        self.od_var = od_var
        self.s_label = s_label
        self.l_label = l_label
        self.units = units
        self.des_d_type = "single"
        self.od_d_type = "list"
        self.limit_var = ""
        self.limit_var_type = "max"
        self.des = -999
        self.od = -999
        self.y_label_style = ""
        self.y_axis_min_max = ""
        self.ticks = ""
        self.minloc = 2
        
    def print_des_od_comp(self, dict_results, i_od):
        if(self.des_var == "none"):
            des = self.des
        else:
            des = dict_results[self.des_var]
            
        if(self.od_var == "none"):
            od = self.od
        else:
            od = dict_results[self.od_var][i_od]
        
        print(self.l_label, "\nDes:", des, "\nOD: ", od)

def get_des_od_label_unit_info__combined():
    
    calc_metrics_dict = get_des_od_label_unit_info__calc_metrics()
    
    out_dict = calc_metrics_dict.copy()
    out_dict.update(get_des_od_label_unit_info__ind_inputs())
    
    return out_dict

def get_des_od_mspt_label_unit_info__combined():
    
    out_dict_temp = get_des_od_label_unit_info__calc_metrics()
    out_dict = out_dict_temp.copy()
    
    out_dict.update(get_des_od_label_unit_info__ind_inputs())
    out_dict.update(get_sam_mspt_sco2_label_unit_info())
    
    return out_dict

def get_sam_mspt_sco2_label_unit_info():
    
    info = {}
    info["sam_recup_tot_UA"] = C_des_od_label_unit_info("sam_UA_recup_total", "none", "Total Recup\nUA [MW/K]", "Total Recuperator Conductance [MW/K]", "[MW/K]")
    info["E_tot_derated"] = C_des_od_label_unit_info("annual_energy_derated", "none", "Annual Energy\nDerated [kWh]", "Annual Energy Derated [kWh]", "[kWh]")
    info["E_tot_derated"].y_label_style = "sci"
    info["lcoe_nom"] = C_des_od_label_unit_info("lcoe_nom", "none", "LCOE\n[cents/kWh]", "LCOE [cents/kWh]", "[cents/kWh]")
    info["TES_cost"] = C_des_od_label_unit_info("TES_cost", "none", "TES\nCost [M$]", "Thermal Energy Storage Cost [M$]", "[M$]")
    info["sam_cycle_cost"] = C_des_od_label_unit_info("sam_cycle_cost", "none", "Cycle\nCost [M$]", "Cycle Cost [M$]", "[M$]")
    info["mspt_tot_installed_cost"] = C_des_od_label_unit_info("mspt_tot_installed_cost", "none", "MSPT\nCost [M$]", "MSPT System Installed Cost [M$]", "[M$]")
    info["rec_eta_therm"] = C_des_od_label_unit_info("rec_eta_therm", "none", "Receiver\nEfficiency [-]", "Receiver Thermal Efficiency [-]", "[-]")
    info["E_tot_cycle"] = C_des_od_label_unit_info("annual_cycle_output", "none", "Annual\nEnergy [kWh]", "Annual Energy [kWh]", "[kWh]")
    info["E_tot_cycle"].y_label_style = "sci"
    info["E_rec_pump_power"] = C_des_od_label_unit_info("annual_rec_pump_power", "none", "Annual Receiver\nPumping Energy[kWh]", "Annual Receiver Pumping Energy [kWh]", "[kWh]")
    info["E_rec_pump_power"].y_label_style = "sci"
    info["rec_pump_perc"] = C_des_od_label_unit_info("rec_pump_perc", "none", "Rec Pump Frac\nTot Energy [%]", "Rec Pump Fraction of Total Energy Ouput [%]", "[%]")
    info["solar_mult"] = C_des_od_label_unit_info("solar_mult", "none", "Solar\nMultiple [-]", "Solar Multiple [-]", "[-]")
    info["TES_capacity"] = C_des_od_label_unit_info("Q_TES", "none", "TES\nCapacity [MWh]", "TES Capacity [MWh]", "[MWh]")
    info["TES_capacity"].y_label_style = "sci"
    info["TES_volume"] = C_des_od_label_unit_info("V_TES", "none", "TES\nVolume [m3]", "TES Volume [m3]", "[m3]")
    info["TES_volume"].y_label_style = "sci"
    info["sam_phx_deltat"] = C_des_od_label_unit_info("sam_phx_deltat", "none", "PHX HTF\nTemp Diff [C]", "PHX HTF Temperature Difference [C]", "[C]")
    info["sam_T_htf_cold"] = C_des_od_label_unit_info("sam_T_htf_cold", "none", "HTF Cold\nTemp [C]", "HTF Cold Return Temperature [C]", "[C]")
    info["sam_eta_cycle"] = C_des_od_label_unit_info("sam_eta_cycle", "none", "Cycle\nEfficiency [-]", "Cycle Thermal Efficiency [-]", "[-]")
    info["annual_rec_delivered"] = C_des_od_label_unit_info("annual_rec_delivered", "none", "Annual Rec\nOutput [kWh]", "Annual Receiver Output [kWh]", "[kWh]")
    info["annual_rec_delivered"].y_label_style = "sci"
    info["m_dot_htf_design"] = C_des_od_label_unit_info("m_dot_rec_design", "none", "Rec Design\nMass Flow [kg/s]", "Receiver Design Mass Flow Rate [kg/s]", "[kg/s]")    
    info["m_dot_htf_design"].y_label_style = "sci"
    info["annual_rec_defocused"] = C_des_od_label_unit_info("annual_rec_defocused", "none", "Rec Defocus\nEnergy [kWh]", "Receiver Total Defocus Energy [kWh]", "[kWh]")
    info["annual_rec_defocused"].y_label_style = "sci"
    
    return info
    

def get_des_od_label_unit_info__calc_metrics():
    
    info = {}
    info["W_dot"] = C_des_od_label_unit_info("W_dot_net_des", "W_dot_net_od", "Net\nPower [MWe]", "Net Power [MWe]", "[MWe]")
    info["W_dot"].limit_var = "W_dot_net_des"
    info["eta"] = C_des_od_label_unit_info("eta_thermal_calc", "eta_thermal_od", "Thermal\nEfficiency [-]", "Thermal Efficiency [-]", "[-]")
    info["Q_dot"] = C_des_od_label_unit_info("q_dot_PHX", "Q_dot_od", "Thermal\nInput [MWt]", "Thermal Input [MWt]", "[MWt]")   
    info["min_phx_deltaT"] = C_des_od_label_unit_info("min_phx_deltaT", "none", "Min PHX HTF\nTemp Diff [C]", "Min Allowable PHX HTF Temp Diff [C]", "[C]")
    info["min_phx_deltaT"].od_d_type = "nan"
    info["f_recomp"] = C_des_od_label_unit_info("recomp_frac", "recomp_frac_od", "Recompression\nFraction [-]", "Recompression Fraction [-]", "[-]")
    info["cycle_cost"] = C_des_od_label_unit_info("cycle_cost", "none", "Cycle\nCost [M$]", "Cycle Cost [M$]", "[M$]")
    info["cycle_cost"].od_d_type = "nan"
    info["cycle_spec_cost"] = C_des_od_label_unit_info("cycle_spec_cost", "none", "Cycle Spec\nCost [$/kWe]", "Cycle Specific Cost [$/kWe]", "[$/kWe]")
    info["cycle_spec_cost"].od_d_type = "nan"
    info["cycle_spec_cost_thermal"] = C_des_od_label_unit_info("cycle_spec_cost_thermal", "none", "Cycle Spec\nCost [$/kWt]", "Cycle Specific Cost [$/kWt]", "[kWt]")
    info["cycle_spec_cost_thermal"].od_d_type = "nan"
    info["T_htf_cold"] = C_des_od_label_unit_info("T_htf_cold_des", "T_htf_cold_od", "HTF Cold\nReturn Temp [C]", "HTF Cold Return Temperature [C]", "[C]")
    
    info["MC_T_in"] = C_des_od_label_unit_info("T_comp_in", "T_mc_in_od", "Main Comp\nInlet Temp [C]", "Main Compressor Inlet Temp [C]", "[C]")
    info["MC_P_in"] = C_des_od_label_unit_info("P_comp_in", "P_comp_in_od", "Main Comp\nInlet Pres [MPa]", "Main Compressor Inlet Pres [MPa]", "[MPa]")
    info["MC_P_out"] = C_des_od_label_unit_info("P_comp_out", "P_mc_out_od", "Main Comp\nOutlet Pres [MPa]", "Main Compressor Outlet Pres [MPa]", "[MPa]")
    info["MC_P_out"].limit_var = "P_high_limit"
    info["MC_T_out"] = C_des_od_label_unit_info("mc_T_out", "mc_T_out_od", "Main Comp\nOutlet Temp [C]", "Main Compressor Outlet Temp [C]", "[C]")
    info["MC_W_dot"] = C_des_od_label_unit_info("mc_W_dot", "mc_W_dot_od", "Main Comp\nPower [MWe]", "Main Compressor Power [MWe]", "[MWe]")
    info["MC_rho_in"] = C_des_od_label_unit_info("mc_rho_in", "mc_rho_in_od", "Main Comp\nInlet Density [kg/m3]", "Main Compressor Inlet Density [kg/m3]", "[kg/m3]")
    info["MC_m_dot"] = C_des_od_label_unit_info("mc_m_dot_des", "mc_m_dot_od", "Main Comp\nMass Flow [kg/s]", "Main Compressor Mass Flow Rate [kg/s]", "[kg/s]")
    info["MC_ideal_spec_work"] = C_des_od_label_unit_info("mc_ideal_spec_work", "mc_ideal_spec_work_od", "Main Comp\nIsen Spec Work [kJ/kg]", "Main Compressor Isen Spec Work [kJ/kg]", "[kJ/kg]")
    info["MC_phi"] = C_des_od_label_unit_info("mc_phi_des", "mc_phi_od", "Main Comp\nFlow Coef [-]", "Main Compressor Flow Coefficient [-]", "[-]")
    info["MC_phi"].limit_var = "mc_phi_surge"
    info["MC_phi"].limit_var_type = "min"
    info["MC_psi"] = C_des_od_label_unit_info("mc_psi_des", "mc_psi_od", "Main Comp\nIdeal Head Coef [-]", "Main Compressor Ideal Head Coef[-]", "[-]")
    info["MC_psi"].limit_var = "mc_psi_max_at_N_des"
    info["MC_psi"].limit_var_type = "max"
    info["MC_tip_speed"] = C_des_od_label_unit_info("mc_tip_ratio_des", "mc_tip_ratio_od", "Main Comp\nTip Speed Ratio [-]", "Main Compressor Tip Speed Ratio [-]", "[-]")
    info["MC_tip_speed"].des_d_type = "list"
    info["MC_tip_speed"].od_d_type = "matrix"
    info["MC_N"] = C_des_od_label_unit_info("mc_N_des", "mc_N_od", "Main Comp\nSpeed [rpm]", "Main Compressor Shaft Speed [rpm]", "[rpm]")
    info["MC_eta"] = C_des_od_label_unit_info("eta_isen_mc", "mc_eta_od", "Main Comp\nIsen Eff. [-]", "Main Compressor Isentropic Efficiency [-]", "-")
    info["MC_eta_stages"] = C_des_od_label_unit_info("mc_eta_stages_des", "mc_eta_stages_od", "MC Stages\nIsen Eff. [-]", "MC Stages Isentropic Efficiency [-]", "-")
    info["MC_eta_stages"].des_d_type = "list"
    info["MC_eta_stages"].od_d_type = "matrix"
    info["MC_bypass"] = C_des_od_label_unit_info("none","mc_f_bypass_od", "MC Flow\nBypass [-]", "MC Flow Bypass [-]", "[-]")
    info["MC_bypass"].des = 0
    info["MC_cost"] = C_des_od_label_unit_info("mc_cost", "none", "Main Comp\nCost [M$]", "Main Compressor Cost [M$]", "[M$]")
    info["MC_cost"].od_d_type = "nan"
    
    info["RC_T_in"] = C_des_od_label_unit_info("rc_T_in_des", "rc_T_in_od", "Re-Comp\nInlet Temp [C]", "Re-Compressor Inlet Temp [C]", "[C]")
    info["RC_P_in"] = C_des_od_label_unit_info("rc_P_in_des", "rc_P_in_od", "Re-Comp\nInlet Pres [MPa]", "Re-Compressor Inlet Pres [MPa]","[MPa]")
    info["RC_T_out"] = C_des_od_label_unit_info("rc_T_out_des", "rc_T_out_od", "Re-Comp\nOutlet Temp [C]", "Re-Compressor Outlet Temp [C]", "[C]")
    info["RC_P_out"] = C_des_od_label_unit_info("rc_P_out_des", "rc_P_out_od", "Re-Comp\nOutlet Pres [MPa]", "Re-Compressor Outlet Pres [MPa]", "[MPa]")
    info["RC_W_dot"] = C_des_od_label_unit_info("rc_W_dot", "rc_W_dot_od", "Re-Comp\nPower [MWe]", "Re-Compressor Power [MWe]", "[MWe]")
    info["RC_m_dot"] = C_des_od_label_unit_info("rc_m_dot_des", "rc_m_dot_od", "Re-Comp\nMass Flow [kg/s]", "Re-Compressor Mass Flow Rate [kg/s]", "[kg/s]")
    info["RC_phi"] = C_des_od_label_unit_info("rc_phi_des", "rc_phi_od", "Re-Comp\nFlow Coef [-]", "Re-Compressor Flow Coefficient [-]", "[-]")
    info["RC_phi"].limit_var = "rc_phi_surge"
    info["RC_phi"].limit_var_type = "min"
    info["RC_psi"] = C_des_od_label_unit_info("rc_psi_des", "rc_psi_od", "Re-Comp\nIdeal Head Coef [-]",
                                              "Re-Compressor Ideal Head Coefficient [-]", "[-]")
    info["RC_psi"].limit_var = "rc_psi_max_at_N_des"
    info["RC_psi"].limit_var_type = "max"
    info["RC_tip_speed"] = C_des_od_label_unit_info("rc_tip_ratio_des", "rc_tip_ratio_od", "Re-Comp\nTip Speed Ratio [-]", "Re-Compressor Tip Speed Ratio [-]", "[-]")
    info["RC_tip_speed"].des_d_type = "list"
    info["RC_tip_speed"].od_d_type = "matrix"
    info["RC_N"] = C_des_od_label_unit_info("rc_N_des", "rc_N_od", "Re-Comp\nSpeed [rpm]", "Re-Compressor Shaft Speed [rpm]", "[rpm]")
    info["RC_eta"] = C_des_od_label_unit_info("eta_isen_rc", "rc_eta_od", "Re-Comp\nIsen Eff. [-]", "Recompressor Isentropic Efficiency [-]", "-")
    info["RC_eta_stages"] = C_des_od_label_unit_info("rc_eta_stages_des", "rc_eta_stages_od", "RC Stages\nIsen Eff. [-]", "RC Stages Isentropic Efficiency [-]", "-")
    info["RC_cost"] = C_des_od_label_unit_info("rc_cost", "none", "Re-Comp\nCost [M$]", "Recompressor Cost [M$]", "[M$]")
    info["RC_cost"].od_d_type = "nan"
    
    info["PC_T_in"] = C_des_od_label_unit_info("pc_T_in_des", "pc_T_in_od", "Pre-Comp\nInlet Temp [C]", "Pre-Compressor Inlet Temp [C]", "[C]")
    info["PC_P_in"] = C_des_od_label_unit_info("pc_P_in_des", "pc_P_in_od", "Pre-Comp\nInlet Pres [MPa]", "Pre-Compressor Inlet Pres [MPa]", "[MPa]")
    info["PC_W_dot"] = C_des_od_label_unit_info("pc_W_dot", "pc_W_dot_od", "Pre-Comp\nPower [MWe]", "Pre-Compressor Power [MWe]", "[MWe]")
    info["PC_m_dot"] = C_des_od_label_unit_info("pc_m_dot_des", "pc_m_dot_od", "Pre-Comp\nMass Flow [kg/s]", "Pre-Compressor Mass Flow Rate [kg/s]", "[kg/s]")
    info["PC_phi"] = C_des_od_label_unit_info("pc_phi_des", "pc_phi_od", "Pre-Comp\nFlow Coef [-]", "Pre-Compressor Flow Coefficient [-]", "[-]")
    info["PC_phi"].limit_var = "pc_phi_surge"
    info["PC_phi"].limit_var_type = "min"
    info["PC_tip_speed"] = C_des_od_label_unit_info("pc_tip_ratio_des", "pc_tip_ratio_od", "Pre-Comp\nTip Speed Ratio [-]", "Pre-Compressor Tip Speed Ratio [-]", "[-]")
    info["PC_tip_speed"].des_d_type = "list"
    info["PC_tip_speed"].od_d_type = "matrix"
    info["PC_N"] = C_des_od_label_unit_info("pc_N_des", "pc_N_od", "Pre-Comp\nSpeed [rpm]", "Pre-Compressor Shaft Speed [rpm]", "[rpm]")
    info["PC_eta"] = C_des_od_label_unit_info("eta_isen_pc", "pc_eta_od", "Pre-Comp\nIsen Eff. [-]", "Precompressor Isentropic Efficiency [-]", "-")
    info["PC_eta_stages"] = C_des_od_label_unit_info("pc_eta_stages_des", "pc_eta_stages_od", "PC Stages\nIsen Eff. [-]", "PC Stages Isentropic Efficiency [-]", "-")
    info["PC_bypass"] = C_des_od_label_unit_info("none","pc_f_bypass_od", "PC Flow\nBypass [-]", "PC Flow Bypass [-]", "[-]")
    info["PC_bypass"].des = 0
    info["PC_cost"] = C_des_od_label_unit_info("pc_cost", "none", "Pre-Comp\nCost [M$]", "Precompressor Cost [M$]", "[M$]")
    info["PC_cost"].od_d_type = "nan"
    
    info["c_tot_cost"] = C_des_od_label_unit_info("c_tot_cost", "none", "Total Comp\nCost [M$]", "Total Compressor Cost [M$]", "[M$]")
    info["c_tot_cost"].od_d_type = "nan"
    info["c_tot_W_dot"] = C_des_od_label_unit_info("c_tot_W_dot", "c_tot_W_dot_od", "Total Comp\nPower [MWe]", "Total Compressor Power [MWe]", "[MWe]")
    #info["c_tot_W_dot"].od_d_type = "nan"
        
    info["t_T_in"] = C_des_od_label_unit_info("T_turb_in", "T_co2_PHX_out_od", "Turbine\nInlet Temp [C]", "Turbine Inlet Temperature [C]", "[C]")
    info["t_T_in"].limit_var = "T_htf_hot_des"
    info["t_P_in"] = C_des_od_label_unit_info("t_P_in_des", "t_P_in_od", "Turbine\nInlet Pres [MPa]", "Turbine Inlet Pressure [MPa]", "[MPa]")
    info["t_T_out"] = C_des_od_label_unit_info("t_T_out_des", "t_T_out_od", "Turbine\nOutlet Temp [C]", "Turbine Outlet Temperature [C]", "[C]")
    info["t_P_out"] = C_des_od_label_unit_info("t_P_out_des", "t_P_out_od", "Turbine\nOutlet Pres [MPa]", "Turbine Outlet Pressure [MPa]", "[MPa]")
    info["t_W_dot"] = C_des_od_label_unit_info("t_W_dot", "t_W_dot_od", "Turbine\nPower [MWe]", "Turbine Power [MWe]", "[MWe]")
    info["t_m_dot"] = C_des_od_label_unit_info("t_m_dot_des", "t_m_dot_od", "Turbine\nMass Flow [kg/s]", "Turbine Mass Flow Rate [kg/s]", "[kg/s]")
    info["t_nu"] = C_des_od_label_unit_info("t_nu_des", "t_nu_od", "Turbine\nVelocity Ratio [-]", "Turbine Velocity Ratio [-]", "[-]")
    info["t_tip_speed"] = C_des_od_label_unit_info("t_tip_ratio_des", "t_tip_ratio_od", "Turbine\nTip Speed Ratio [-]", "Turbine Tip Speed Ratio [-]", "[-]")
    info["t_N"] = C_des_od_label_unit_info("t_N_des", "t_N_od", "Turbine\nSpeed [rpm]", "Turbine Shaft Speed [rpm]", "[rpm]")
    info["t_eta"] = C_des_od_label_unit_info("eta_isen_t", "t_eta_od", "Turbine\nEfficiency [-]", "Turbine Efficiency [-]", "[-]")
    info["t_cost"] = C_des_od_label_unit_info("t_cost", "none", "Turbine\nCost [M$]", "Turbine Cost [M$]", "[M$]")
    info["t_cost"].od_d_type = "nan"

    info["LTR_HP_T_out"] = C_des_od_label_unit_info("LTR_HP_T_out_des", "LTR_HP_T_out_od", "LTR HP\nOutlet Temp [C]", "LTR HP Outlet Temperature [C]", "[C]")
    info["LTR_UA"] = C_des_od_label_unit_info("LTR_UA_assigned", "none", "LTR\nUA Assigned [MW/K]", "LTR Conductance Assigned [MW/K]", "[MW/K]")
    info["LTR_UA"].od_d_type = "nan"
    info["LTR_UA_calculated"] = C_des_od_label_unit_info("LTR_UA_calculated", "none", "LTR\nUA Calculated [MW/K]", "LTR Conductance Calculated [MW/K]", "[MW/K]")
    info["LTR_UA_calculated"].od_d_type = "nan"
    info["LTR_eff"] = C_des_od_label_unit_info("eff_LTR", "eff_LTR_od", "LTR\nEffectiveness [-]", "Low Temp Recuperator Effectiveness [-]", "[-]")
    info["LTR_q_dot"] = C_des_od_label_unit_info("q_dot_LTR", "q_dot_LTR_od", "LTR\nDuty [MWt]", "Low Temp Recuperator Duty [MWt]", "[MWt]")
    info["LTR_LP_deltaP"] = C_des_od_label_unit_info("LTR_LP_deltaP_des", "LTR_LP_deltaP_od", "LTR LP\nPres Drop [-]", "LTR LP Pressure Drop [-]", "[-]")
    info["LTR_HP_deltaP"] = C_des_od_label_unit_info("LTR_HP_deltaP_des", "LTR_HP_deltaP_od", "LTR HP\nPres Drop [-]", "LTR HP Pressure Drop [-]", "[-]")
    info["LTR_min_dT"] = C_des_od_label_unit_info("LTR_min_dT", "LTR_min_dT_od", "LTR Min\nTemp Difference [C]", "LTR Min Temperature Difference [C]", "[C]")
    info["LTR_cost"] = C_des_od_label_unit_info("LTR_cost", "none", "LTR\nCost [M$]", "Low Temp Recuperator Cost [M$]", "[M$]")
    info["LTR_cost"].od_d_type = "nan"
    
    info["HTR_LP_T_out"] = C_des_od_label_unit_info("HTR_LP_T_out_des", "HTR_LP_T_out_od", "HTR LP\nOutlet Temp [C]", "HTR LP Outlet Temperature [C]", "[C]")
    info["HTR_HP_T_in"] = C_des_od_label_unit_info("HTR_HP_T_in_des", "HTR_HP_T_in_od", "HTR HP\nInlet Temp [C]", "HTR HP Inlet Temperature [C]", "[C]")
    info["HTR_UA"] = C_des_od_label_unit_info("HTR_UA_assigned", "none", "HTR\nUA Assigned [MW/K]", "HTR Conductance Assigned [MW/K]", "[MW/K]")
    info["HTR_UA"].od_d_type = "nan"
    info["HTR_UA_calculated"] = C_des_od_label_unit_info("HTR_UA_calculated", "none", "HTR\nUA Calculated [MW/K]", "HTR Conductance Calculated [MW/K]", "[MW/K]")
    info["HTR_UA_calculated"].od_d_type = "nan"
    info["HTR_eff"] = C_des_od_label_unit_info("eff_HTR", "eff_HTR_od", "HTR\nEffectiveness [-]", "High Temp Recuperator Effectiveness [-]", "[-]")
    info["HTR_q_dot"] = C_des_od_label_unit_info("q_dot_HTR", "q_dot_HTR_od", "HTR\nDuty [MWt]", "High Temp Recuperator Duty [MWt]", "[MWt]")
    info["HTR_LP_deltaP"] = C_des_od_label_unit_info("HTR_LP_deltaP_des", "HTR_LP_deltaP_od", "HTR LP\nPres Drop [-]", "HTR LP Pressure Drop [-]", "[-]")
    info["HTR_HP_deltaP"] = C_des_od_label_unit_info("HTR_HP_deltaP_des", "HTR_HP_deltaP_od", "HTR HP\nPres Drop [-]", "HTR HP Pressure Drop [-]", "[-]")
    info["HTR_min_dT"] = C_des_od_label_unit_info("HTR_min_dT", "HTR_min_dT_od", "HTR Min\nTemp Difference [C]", "HTR Min Temperature Difference [C]", "[C]")
    info["HTR_cost"] = C_des_od_label_unit_info("HTR_cost", "none", "HTR\nCost [M$]", "High Temp Recuperator Cost [M$]", "[M$]")
    info["HTR_cost"].od_d_type = "nan"
    
    info["recup_total_cost"] = C_des_od_label_unit_info("recup_total_cost", "none", "Total Recup\n Cost [M$]", "Total Recuperator Cost [M$]", "[M$]")
    info["recup_total_cost"].od_d_type = "nan"
    info["recup_tot_UA"] = C_des_od_label_unit_info("recup_total_UA_assigned", "none", "Total Recup\nUA Assigned [MW/K]", "Total Recuperator Conductance Assigned [MW/K]", "[MW/K]")
    info["recup_tot_UA"].od_d_type = "nan"
    info["recup_tot_UA_calculated"] = C_des_od_label_unit_info("recup_total_UA_calculated", "none", "Total Recup\nUA Calculated [MW/K]", "Total Recuperator Conductance Calculated [MW/K]", "[MW/K]")
    info["recup_tot_UA_calculated"].od_d_type = "nan"
    info["recup_tot_UA"].l_label = "Total Recuperator\nConductance [MW/K]"
    info["recup_LTR_UA_frac"]  = C_des_od_label_unit_info("recup_LTR_UA_frac", "none", "Fraction of Total\nUA to LTR [-]", "Fraction of Total Conductance to LTR [-]", "[-]")
    info["recup_LTR_UA_frac"].od_d_type = "nan"
        
    info["PHX_T_co2_in"] = C_des_od_label_unit_info("T_co2_PHX_in", "T_co2_PHX_in_od", "PHX CO2\nInlet Temp [C]", "PHX CO2 Inlet Temperature [C]", "[C]")
    info["PHX_P_co2_in"] = C_des_od_label_unit_info("P_co2_PHX_in", "P_co2_PHX_in_od", "PHX CO2\nInlet Pres [MPa]", "PHX CO2 Inlet Pressure [MPa]", "[MPa]")
    info["PHX_m_dot_HTF"] = C_des_od_label_unit_info("m_dot_htf_des", "none", "PHX HTF\n Mass Flow Rate [kg/s]", "PHX HTF Mass Flow Rate [kg/s]", "[kg/s]")
    info["PHX_m_dot_HTF"].od_d_type = "nan"
    info["PHX_m_dot_HTF"].y_label_style = "sci"
    info["PHX_UA"] = C_des_od_label_unit_info("UA_PHX", "none", "PHX\nConductance [MW/K]", "PHX Conductance [MW/K]", "[MW/K]")
    info["PHX_UA"].od_d_type = "nan"
    info["PHX_dT"] = C_des_od_label_unit_info("deltaT_HTF_PHX", "deltaT_HTF_PHX_od", "PHX HTF\nTemp Diff [C]", "PHX HTF Temperature Difference [C]", "[C]")
    info["PHX_eff"] = C_des_od_label_unit_info("eff_PHX", "phx_eff_od", "PHX\nEffectivenes [-]", "PHX Effectiveness [-]", "[-]")
    info["PHX_co2_deltaP"] = C_des_od_label_unit_info("PHX_co2_deltaP_des", "phx_co2_deltaP_od", "PHX CO2\nPres Drop [-]", "PHX CO2 Pressure Drop [-]", "[-]")
    info["PHX_cost"] = C_des_od_label_unit_info("PHX_cost", "none", "PHX\nCost [M$]", "PHX Cost [M$]", "[M$]")
    info["PHX_cost"].od_d_type = "nan"
    
    info["LP_cooler_T_in"] = C_des_od_label_unit_info("LP_cooler_T_in", "LP_cooler_T_in_od", "LP Cooler\nInlet Temp [C]", "Low Pressure Cooler Inlet Temp [C]", "[C]")
    info["LP_cooler_in_isen_deltah_to_P_mc_out"] = C_des_od_label_unit_info("LP_cooler_in_isen_deltah_to_P_mc_out", "LP_cooler_in_isen_deltah_to_P_mc_out_od", "LP Cool In\nIsen Spec Work [kJ/kg]", "Low Pressure Cooler Inlet Isen Spec Work [kJ/kg]", "[kJ/kg]")
    info["LP_cooler_rho_in"] = C_des_od_label_unit_info("LP_cooler_rho_in", "LP_cooler_rho_in_od", "LP Cooler\nInlet Density [kg/m3]", "Low Pressure Cooler Inlet Density [kg/m3]", "[kg/m3]")
    info["LP_cooler_co2_deltaP"] = C_des_od_label_unit_info("LP_cooler_co2_deltaP_des", "LP_cooler_co2_deltaP_od", "LP Cooler CO2\nPres Drop [-]", "LP Cooler Pressure Drop [-]", "[-]")
    
    info["LP_cooler_cost"] = C_des_od_label_unit_info("LP_cooler_cost", "none", "LP Cooler\nCost [M$]", "Low Pressure Cooler Cost [M$]", "[M$]")
    info["LP_cooler_cost"].od_d_type = "nan"
    info["LP_cooler_W_dot_fan"] = C_des_od_label_unit_info("LP_cooler_W_dot_fan", "LP_cooler_W_dot_fan_od", "LP Cooler\nFan Power [MWe]", "Low Pressure Cooler Fan Power [MWe]", "[MWe]")
    
    info["IP_cooler_cost"] = C_des_od_label_unit_info("IP_cooler_cost", "none", "IP Cooler\nCost [M$]", "Intermediate Pressure Cooler Cost [M$]", "[M$]")
    info["IP_cooler_cost"].od_d_type = "nan"
    info["IP_cooler_W_dot_fan"] = C_des_od_label_unit_info("IP_cooler_W_dot_fan", "IP_cooler_W_dot_fan_od", "IP Cooler\nFan Power [MWe]", "Intermediate Pressure Cooler Fan Power [MWe]", "[MWe]")
        
    info["cooler_tot_cost"] = C_des_od_label_unit_info("cooler_tot_cost", "none", "Total Cooler\nCosts [M$]", "Total Cooler Costs [M$]", "[M$]")
    info["cooler_tot_cost"].od_d_type = "nan"
    info["cooler_tot_UA"] = C_des_od_label_unit_info("cooler_tot_UA", "none", "Total Cooler\nUA [MW/K]", "Total Cooler Conductance [MW/K]", "[MW/K]")
    info["cooler_tot_UA"].od_d_type = "nan"
    info["cooler_tot_W_dot_fan"] = C_des_od_label_unit_info("cooler_tot_W_dot_fan", "cooler_tot_W_dot_fan_od", "Total Cooler\nFan Power [MWe]", "Total Cooler Fan Power [MWe]", "[MWe]")
    
    return info

def get_des_od_label_unit_info__ind_inputs():
    
    info = {}
    info["T_amb"] = C_des_od_label_unit_info("T_amb_des", "T_amb_od", "Ambient\nTemp [C]", "Ambient Temperature [C]", "[C]")
    info["m_dot_HTF"] = C_des_od_label_unit_info("none", "m_dot_htf_fracs", "Normalized\nMass Flow [-]", "Normalized Mass Flow Rate [-]", "[-]")
    info["m_dot_HTF"].des = 1.0
    info["T_HTF"] = C_des_od_label_unit_info("T_htf_hot_des", "T_htf_hot_od", "HTF Hot\nTemp [C]", "HTF Hot Temperature [C]", "[C]")
    
    return info

def get_cycle_names_for_plot(list_cycle_name):
    
    out_list = list_cycle_name.copy()
    
    for i, name in enumerate(out_list):
        
        if(name == "recompression"):
            out_list[i] = "Recompression"
        elif(name == "partialcooling"):
            out_list[i] = "Partial Cooling"
        elif(name == "simple"):
            out_list[i] = "Simple"
        else:
            out_list[i] = "Unknown"
            
    return out_list

def get_cycle_name_str(cycle_des_par_dict):
    
    cycle_config = cycle_des_par_dict["cycle_config"]
    cycle_is_recomp = cycle_des_par_dict["is_recomp_ok"]
    
    print(cycle_config)
    print(cycle_is_recomp)
    
    if(cycle_config == 1 and cycle_is_recomp == 1):
        return "recompression"
    elif(cycle_config == 1):
        return "simple"
    elif(cycle_config == 2):
        return "partialcooling"
    else:
        return "unknowncycle"

def mod_dict_for_cycle_config(dict_in, cycle_config_str):
    
    dict_out = dict_in.copy()
    
    if(cycle_config_str == "recompression"):
        dict_out["cycle_config"] = 1
        dict_out["is_recomp_ok"] = 1
        
    elif(cycle_config_str == "simple"):
        dict_out["cycle_config"] = 1
        dict_out["is_recomp_ok"] = 0
        
    elif(cycle_config_str == "partialcooling"):
        dict_out["cycle_config"] = 2
        dict_out["is_recomp_ok"] = 1
    
    else:
        dict_out["cycle_config"] = -999
        dict_out["is_recomp_ok"] = -999
                
    return dict_out        
        
def get_default_sco2_dict():
    
    des_par = {}
    des_par["htf"] = 17;                   #[-] Solar salt
    des_par["T_htf_hot_des"] = 670.0;      #[C] HTF design hot temperature (PHX inlet)
    des_par["dT_PHX_hot_approach"] = 20.0; #[C/K] Temperature difference between hot HTF and turbine inlet       
    des_par["T_amb_des"] = 35.0;           #[C] Ambient temperature at design 
    des_par["dT_mc_approach"] = 5.0;       #[C] Temperature difference between main compressor CO2 inlet and ambient air
    des_par["site_elevation"] = 300;       #[m] Used to size air cooler...
    des_par["W_dot_net_des"] = 115.0;      #[MWe] Design cycle power output (no cooling parasitics)
    
    des_par["design_method"] = 2;          #[-] 1 = specify efficiency, 2 = specify total recup UA, 3 = Specify each recup design (see inputs below)
    des_par["eta_thermal_des"] = 0.46;     #[-] Power cycle thermal efficiency
    des_par["UA_recup_tot_des"] = 30000/115.0*des_par["W_dot_net_des"]   #[kW/K] Total recuperator conductance
    
    des_par["cycle_config"] = 1            #[1] = RC, [2] = PC
           
    des_par["is_recomp_ok"] = 1;           #[-] 1 = Yes, 0 = simple cycle only
    des_par["is_P_high_fixed"] = 0;        #[-] 0 = No, optimize. 1 = Yes
    des_par["is_PR_fixed"] = 0;            #[-] 0 = No, >0 = Yes
    des_par["is_IP_fixed"] = 0;
	
    des_par["des_objective"] = 1;          #[-] 2 = hit min deltaT then max efficiency, != 2 = max efficiency
    des_par["min_phx_deltaT"] = 1000;      #[C] Min allowable deltaT across PHX
    des_par["rel_tol"] = 3;                #[-] Baseline solver and optimization relative tolerance exponent (10^-rel_tol)
           
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
    recup_eff_max = 1.0
    deltaP_recup_HP = 0.0056  # [-] = 0.14[MPa]/25[MPa]
    deltaP_recup_LP = 0.0311  # [-] = 0.28[MPa]/9[MPa]
    # LTR
    des_par["LTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["LTR_UA_des_in"] = 2200.0     # [kW/K] (required if LTR_design_code == 1)
    des_par["LTR_min_dT_des_in"] = 12.0   # [C] (required if LTR_design_code == 2)
    des_par["LTR_eff_des_in"] = 0.895     # [-] (required if LTR_design_code == 3)
    des_par["LT_recup_eff_max"] = recup_eff_max    # [-] Maximum effectiveness low temperature recuperator
    des_par["LTR_LP_deltaP_des_in"] = deltaP_recup_LP  # [-]
    des_par["LTR_HP_deltaP_des_in"] = deltaP_recup_HP  # [-]
    # HTR
    des_par["HTR_design_code"] = 3        # 1 = UA, 2 = min dT, 3 = effectiveness
    des_par["HTR_UA_des_in"] = 2800.0     # [kW/K] (required if LTR_design_code == 1)
    des_par["HTR_min_dT_des_in"] = 19.2   # [C] (required if LTR_design_code == 2)
    des_par["HTR_eff_des_in"] = 0.935     # [-] (required if LTR_design_code == 3)
    des_par["HT_recup_eff_max"] = recup_eff_max  # [-] Maximum effectiveness high temperature recuperator
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
	
    return des_par
 
class C_sco2_sim:
    
    def __init__(self, config):
        self.m_des_par_default = get_default_sco2_dict()        
        self.m_des_par_base = self.m_des_par_default       # Can overwrite default with method below
        self.m_des_par_base["cycle_config"]= config
        self.m_des_par_sim = self.m_des_par_base
        self.m_is_print_key_outputs = True
        self.m_solve_success = np.nan
        self.m_solve_dict = np.nan
        self.m_par_solve_success = np.nan
        self.m_par_solve_dict = np.nan
        self.m_also_save_csv = False
        
    def overwrite_default_design_parameters(self, dict_in):
        for key in dict_in:
            self.m_des_par_base[key] = dict_in[key]
                
    def reset_des_par_base_to_default_RC(self):
        self.m_des_par_base = self.m_des_par_default
        self.m_des_par_base["cycle_config"] = 1
                           
    def reset_des_par_base_to_default_PC(self):
        self.m_des_par_base = self.m_des_par_default
        self.m_des_par_base["cycle_config"] = 2
    
    def overwrite_des_par_base(self, partial_dict_in):
        
        for key in partial_dict_in:
            self.m_des_par_base[key] = partial_dict_in[key]
        
    def overwrite_des_par_base_to_partialcooling(self):
        self.m_des_par_base["cycle_config"] = 2
                           
    def overwrite_des_par_base_to_recompression(self):
        self.m_des_par_base["cycle_config"] = 1
        
    def new_des_par_sim_from_base(self, partial_dict_in):
        
        self.m_des_par_sim = self.m_des_par_base
        
        for key in partial_dict_in:
            self.m_des_par_sim[key] = partial_dict_in[key]
            
        # Changing design parameters, so nan the 'solve' members
        self.m_solve_success = np.nan
        self.m_solve_dict = np.nan
    
    def ow_des_par_base__od_P_mc_in_sweep__default(self):
        
        T_htf_des = self.m_des_par_base["T_htf_hot_des"]
        
        T_amb_des = self.m_des_par_base["T_amb_des"]
        
        self.ow_des_par_base__od_P_mc_in_sweep(T_htf_des, 1.0, T_amb_des)
        
    def ow_des_par_base__od_P_mc_in_sweep__T_amb_od(self, T_amb_od):
        
        T_htf_des = self.m_des_par_base["T_htf_hot_des"]
        
        self.ow_des_par_base__od_P_mc_in_sweep(T_htf_des, 1.0, T_amb_od)
        
    def ow_des_par_base__od_P_mc_in_sweep(self, T_htf_od, m_dot_od, T_amb_od):
        
        list_od_P_mc_in_sweep = [T_htf_od, m_dot_od, T_amb_od, 4, 1.E-3]
        
        mod_base_dict = {"od_P_mc_in_sweep": list_od_P_mc_in_sweep}
        
        self.overwrite_des_par_base(mod_base_dict)
    
    def ow_des_par_base__m_dot_htf_par__T_amb_od__T_htf_des(self, list_m_dot_htf_ND_od, T_amb_od):
        
        T_htf_des = self.m_des_par_base["T_htf_hot_des"]
        
        list_od_cases = [[T_htf_des, list_m_dot_htf_ND_od[0], T_amb_od, 4, 1.E-3]]
        
        n_m_dot_htf_od = len(list_m_dot_htf_ND_od)
        
        for i in range(1, n_m_dot_htf_od):
            list_od_cases.append([T_htf_des, list_m_dot_htf_ND_od[i], T_amb_od, 4, 1.E-3])
        
        mod_base_dict = {"od_cases" : list_od_cases}
        
        self.overwrite_des_par_base(mod_base_dict)
    
    def ow_des_par_base_T_amb_od_par_m_dot_T_htf_des(self, list_T_amb_od):
        
        T_htf_des = self.m_des_par_base["T_htf_hot_des"]
        
        list_od_cases = [[T_htf_des, 1.0, list_T_amb_od[0], 4, 1.E-3]]
        
        n_T_amb_od = len(list_T_amb_od)
        
        for i in range(1, n_T_amb_od, 1):
            list_od_cases.append([T_htf_des, 1.0, list_T_amb_od[i], 4, 1.E-3])
        
        mod_base_dict = {"od_cases" : list_od_cases}
        
        self.overwrite_des_par_base(mod_base_dict)  
    
    def ow_des_par_base_T_amb_od_par(self, list_T_amb_od, m_dot_ND_od, T_htf_od):
        
        list_od_cases = [[T_htf_od, m_dot_ND_od, list_T_amb_od[0], 4, 1.E-3]]
        
        n_T_amb_od = len(list_T_amb_od)
        
        for i in range(1, n_T_amb_od, 1):
            list_od_cases.append([T_htf_od, m_dot_ND_od, list_T_amb_od[i], 4, 1.E-3])
        
        mod_base_dict = {"od_cases" : list_od_cases}
        
        self.overwrite_des_par_base(mod_base_dict)  
        
    def ow_des_par_base_set_one_od_case_at_des(self):
        
        T_htf_des = self.m_des_par_base["T_htf_hot_des"]
        T_amb_des = self.m_des_par_base["T_amb_des"]
        
        self.overwrite_des_par_base({"od_cases" : [[T_htf_des, 1.0, T_amb_des, 4, 1.E-3]]})
            
    def solve_sco2_case(self):
    
        start_time = time.time()
    
        sco2_return = ssc_sim.cmod_sco2_csp_system(self.m_des_par_sim)
        self.m_solve_success = sco2_return[0]
        self.m_solve_dict = sco2_return[1]
        
        end_time = time.time()
        self.m_solve_dict["solution_time"] = end_time - start_time
        
        if(self.m_is_print_key_outputs):
            print("\nsolve time = ", end_time - start_time)
            print("min_deltaT = ", self.m_solve_dict["min_phx_deltaT"])
            print("UA = ", self.m_solve_dict["UA_recup_tot_des"])
            print("The calculated cycle efficiency is = ", self.m_solve_dict["eta_thermal_calc"])
            print("The recompression fraction is = ", self.m_solve_dict["recomp_frac"]);
            print("The LTR effectiveness is = ", self.m_solve_dict["eff_LTR"])
            print("The LTR NTU is = ", self.m_solve_dict["NTU_LTR"])
            print("The HTR effectiveness is = ", self.m_solve_dict["eff_HTR"])
            print("The HTR NTRU is = ", self.m_solve_dict["NTU_HTR"])
            print("The PHX temp diff = ", self.m_solve_dict["deltaT_HTF_PHX"])
            print("")
                
    def solve_sco2_parametric(self, list_of_partial_dicts_in):
    
        solve_results = ssc_sim.ssc_table_numbers_to_dict_empty("sco2_csp_system")
        solve_results["solution_time"] = []
        solve_success = []
        
        for des_par_i in list_of_partial_dicts_in:

            self.new_des_par_sim_from_base(des_par_i)
            
            self.solve_sco2_case()
      
            for key in self.m_solve_dict:
                if key in solve_results:
                    solve_results[key].append(self.m_solve_dict[key])
                else:
                    print("\n" + key + "\n")
        
            solve_success.append(self.m_solve_success)
        
        self.m_par_solve_dict = solve_results
        self.m_par_solve_success = solve_success
        
    def save_m_solve_dict(self, s_file_name):
        
        j_output = open(s_file_name + ".json", 'w')
            
        json.dump(self.m_solve_dict, j_output)
        j_output.close()
        
        if(self.m_also_save_csv):
            with open(s_file_name+'.csv','w', newline='') as f:
                w = csv.writer(f)
                write_dictionary_to_csv(self.m_solve_dict, w)
                #w.writerows(self.m_solve_dict.items())
            f.close()
    
    def save_m_par_solve_dict(self, s_file_name):
        
        j_output = open(s_file_name + ".json", 'w')
            
        json.dump(self.m_par_solve_dict, j_output)
        j_output.close()
        if(self.m_also_save_csv):
            with open(s_file_name+'.csv','w', newline='') as f:
                w = csv.writer(f)
                write_dictionary_to_csv(self.m_par_solve_dict, w)
                #w.writerows(self.m_par_solve_dict.items())
            f.close()
        
        
    def solve_sco2_UA_parametric(self, list_UA_parametrics):
    
        UA_par_dict = []
        for UA in list_UA_parametrics:
            UA_par_dict.append({"UA_recup_tot_des" : UA})
    
        self.solve_sco2_parametric(UA_par_dict)

    def solve_sco2_deltaT_UA_parametrics(self, deltaT_list, UA_list):
    
        par_dict = []
        for deltaT in deltaT_list:
            
            for UA in UA_list:
                
                par_dict.append({"UA_recup_tot_des" : UA, 
                "min_phx_deltaT" : deltaT, 
                "des_objective" : 2})
        
        self.solve_sco2_parametric(par_dict)
        
    
    def solve_sco2__T_amb_od_par(self, T_amb_des, T_amb_od_cold, T_amb_od_hot, T_amb_od_step_max = 1):

        # Overwrite des par base with 'T_amb_des'
        mod_base_dict = {"T_amb_des" : T_amb_des}
        self.overwrite_des_par_base(mod_base_dict)
    
        # Find cold side delta_T that is equal to or less than max step
        n_cold = np.ceil((T_amb_des - T_amb_od_cold) / T_amb_od_step_max)
        delta_T_cold = (T_amb_des - T_amb_od_cold) / n_cold
        od_cold = list(np.arange(T_amb_od_cold, T_amb_des, delta_T_cold))
    
        # Find hot side delta_T that is equal to or less than max step
        n_hot = np.ceil((T_amb_od_hot - T_amb_des) / T_amb_od_step_max)
        delta_T_hot = (T_amb_od_hot - T_amb_des) / n_hot          
        od_hot = list(np.arange(T_amb_des, T_amb_od_hot + 0.01, delta_T_hot))
        
        list_od_temps = od_cold + od_hot
        print(list_od_temps)
        
        self.ow_des_par_base_T_amb_od_par_m_dot_T_htf_des(list_od_temps)
        
        self.solve_sco2_case()
        
    def solve_sco2__m_dot_htf_od_par__T_amb_od(self, T_amb_od, m_dot_htf_ND_low, m_dot_htf_ND_high, m_dot_htf_ND_step_max = 0.1):

        # Find cold side delta_m_dot that is equal to or less than max step
        n_m_dot_low = np.ceil(np.round(((1.0 - m_dot_htf_ND_low) / m_dot_htf_ND_step_max),decimals = 2))
        delta_m_dot = (1.0 - m_dot_htf_ND_low) / n_m_dot_low
        od_low = list(np.arange(m_dot_htf_ND_low, 1.0, delta_m_dot))
        
        # Find hot side delta_m_dot that is equal to or less than max step
        if(m_dot_htf_ND_high == 1.0):
            od_high = [1.0]
        else:
            n_m_dot_high = np.ceil((m_dot_htf_ND_high - 1.0) / m_dot_htf_ND_step_max)
            delta_m_dot = (m_dot_htf_ND_high - 1.0) / n_m_dot_high
            od_high = list(np.arange(1.0, m_dot_htf_ND_high*1.0001, delta_m_dot))
        
        list_od_m_dots = od_low + od_high
        
        print(list_od_m_dots)
        self.ow_des_par_base__m_dot_htf_par__T_amb_od__T_htf_des(list_od_m_dots, T_amb_od)
        
        self.solve_sco2_case()

    

def solve_default_des_and_compare_od_at_des(cycle_config = 1):
    
    c_sco2 = C_sco2_sim(cycle_config)       # Recompression cycle default
    c_sco2.ow_des_par_base_set_one_od_case_at_des()
    c_sco2.solve_sco2_case()
    cycle_results1 = c_sco2.m_solve_dict
    compare_des_and_od_at_des(cycle_results1)

def sim_data__eta_vs_UA_vs_config(UA_par_in, is_save_data = False, file_name = ""):
    
    c_sco2 = C_sco2_sim(1)
        # Solve Recompression Cycle
    c_sco2.solve_sco2_UA_parametric(UA_par_in)
    RC_des_solved = c_sco2.m_par_solve_dict
        # Solve Partial Cooling Cycle
    c_sco2.overwrite_des_par_base_to_partialcooling()
    c_sco2.solve_sco2_UA_parametric(UA_par_in)
    PC_des_solved = c_sco2.m_par_solve_dict
    
    if(is_save_data):
        
        for cycle_i in [RC_des_solved, PC_des_solved]:
        
            if(cycle_i==RC_des_solved):
                cycle_name = "recompression"
            else:
                cycle_name = "partialcooling"
            
            "Print to json so we can easily read back"
            if(file_name == ""):
                j_output = open(cycle_name + "_UA_parametric.txt",'w')
            else:
                j_output = open(cycle_name + "_" + file_name + "_UA_parametric.txt", 'w')
                
            json.dump(cycle_i, j_output)
            j_output.close()
    
    return RC_des_solved, PC_des_solved

def create_and_save__eta_vs_UA_vs_config(RC_des_solved, PC_des_solved):
    
    # Using ssc solution here, so could also use data from file
    # Would be nice to have some quality check here... (e.g. no matching UAs in a cycle config?)
    RC_des_solved_filtered = cy_plt.filter_dict_column_length(RC_des_solved,len(RC_des_solved["eta_thermal_calc"]))
    PC_des_solved_filtered = cy_plt.filter_dict_column_length(PC_des_solved,len(PC_des_solved["eta_thermal_calc"]))
    
    for i in range(len(RC_des_solved_filtered["eta_thermal_calc"])):
    
        c_sco2_overlay = cy_plt.C_sco2_TS_PH_overlay_plot(cy_plt.filter_dict_to_index(RC_des_solved_filtered,i), cy_plt.filter_dict_to_index(PC_des_solved_filtered,i))
        c_sco2_overlay.is_save_plot = True
        c_sco2_overlay.plot_new_figure()
        #cy_plt.create_new_overlay_TStop_PHbot_figure(cy_plt.filter_dict_to_index(RC_des_solved_filtered,i), cy_plt.filter_dict_to_index(PC_des_solved_filtered,i), True);
        
        c_sco2_ts_ph_RC = cy_plt.C_sco2_TS_PH_plot(cy_plt.filter_dict_to_index(RC_des_solved_filtered,i))
        c_sco2_ts_ph_RC.is_save_plot = True
        c_sco2_ts_ph_RC.plot_new_figure()
        
        c_sco2_ts_ph_PC = cy_plt.C_sco2_TS_PH_plot(cy_plt.filter_dict_to_index(PC_des_solved_filtered,i))
        c_sco2_ts_ph_PC.is_save_plot = True
        c_sco2_ts_ph_PC.plot_new_figure()
                                                     
        #cy_plt.create_new_TStop_PHbot_figure(cy_plt.filter_dict_to_index(RC_des_solved_filtered,i),True)
        #cy_plt.create_new_TStop_PHbot_figure(cy_plt.filter_dict_to_index(PC_des_solved_filtered,i),True)
        
        if(i == 1):
            c_sco2_overlay = cy_plt.C_sco2_TS_PH_overlay_plot(cy_plt.filter_dict_to_index(RC_des_solved_filtered,i), cy_plt.filter_dict_to_index(PC_des_solved_filtered,i-1))
            c_sco2_overlay.is_save_plot = True
            c_sco2_overlay.plot_new_figure()
            #cy_plt.create_new_overlay_TStop_PHbot_figure(cy_plt.filter_dict_to_index(RC_des_solved_filtered,i), cy_plt.filter_dict_to_index(PC_des_solved_filtered,i-1), True);

def sim_data__create_and_save__eta_vs_UA_vs_config(UA_par_in, is_save_data = False, file_name = ""):
    
    RC_des_solved, PC_des_solved = sim_data__eta_vs_UA_vs_config(UA_par_in, is_save_data, file_name)
    
    create_and_save__eta_vs_UA_vs_config(RC_des_solved, PC_des_solved)
    
    return RC_des_solved, PC_des_solved

def load_data__create_and_save__eta_vs_UA_vs_config(rc_json, pc_json):
    
    rc_data = json.load(open(rc_json))
    pc_data = json.load(open(pc_json))
    
    create_and_save__eta_vs_UA_vs_config(rc_data, pc_data)
    
    return rc_data, pc_data

def sim_data__create_and_save__eta_vs_UA_vs_config__sco2_symposium():
    
    UA_par = [20000, 40000]
    sim_data__create_and_save__eta_vs_UA_vs_config(UA_par, True, "sco2_symposium_data")
    
def load_data__create_and_save__eta_vs_UA_vs_config__sco2_symposium():
    
    rc_file = "sco2_symposium_plot_data/recompression_sco2_symposium_data_UA_parametric.txt"
    pc_file = "sco2_symposium_plot_data/partialcooling_sco2_symposium_data_UA_parametric.txt"
    
    load_data__create_and_save__eta_vs_UA_vs_config(rc_file, pc_file)
    
def load_baseline_data():
    
    rc_baseline = json.load(open("baseline_case/recompression_baseline_case_results.txt"))
    pc_baseline = json.load(open("baseline_case/partialcooling_baseline_case_results.txt"))
    
    return rc_baseline, pc_baseline

def load__T_amb_od_par__baseline_RC__data():
    
    T_amb_od_par__baseline_rc = json.load(open("T_amb_od_pars/recompression_T_amb_od_par__baseline__comp.txt"))
    
    return T_amb_od_par__baseline_rc

def compare__T_amb_od_par__input_to_saved(json_par_in):
    
    par_saved = load__T_amb_od_par__baseline_RC__data()
    
    par_input = json.load(open(json_par_in))
    
    compare_dict_files(par_saved, par_input)

def deltaT_UA_config__compare_input_files_to_saved(rc_json, pc_json):
    
    rc_saved, pc_saved = load_baseline_data()
    
    if(rc_json != ""):
        rc_mod = json.load(open(rc_json))
        compare_dict_files(rc_saved, rc_mod)
    
    if(pc_json != ""):
        pc_mod = json.load(open(pc_json))
        compare_dict_files(pc_saved, pc_mod)

def sim_and_save_data__T_amb_od_par__baseline_RC__compare_to_saved():
    
    par_saved = load__T_amb_od_par__baseline_RC__data()
    
    par_mod = sim_and_save_data__T_amb_od_par__baseline_RC()
    
    compare_dict_files(par_saved, par_mod)
    
    return par_mod

def sim_and_save_data__deltaT_UA_config__baseline__compare_to_saved():

    rc_saved, pc_saved = load_baseline_data()

    des_new = sim_and_save_data__deltaT_UA_config__baseline()
    
    is_rc = False
    is_pc = False

    for i in range(len(des_new)):

        if(des_new[i]["cycle_config"][0] == 1):
            rc_new = des_new[i]
            is_rc = True
        elif(des_new[i]["cycle_config"][0] == 2):
            pc_new = des_new[i]
            is_pc = True

    if(is_rc):
        compare_dict_files(rc_saved, rc_new)
    if(is_pc):
        compare_dict_files(pc_saved, pc_new)                
    
def sim_and_save_data__T_amb_od_par__baseline_RC():
    
    T_amb_des = 35
    T_amb_od_cold = 20
    T_amb_od_hot = 50
    T_amb_od_step_max = 1
    cycle_results = sim_and_save_default_new_T_amb__T_amb_od_par("T_amb_od_pars", "T_amb_od_par__baseline", T_amb_des, T_amb_od_cold, T_amb_od_hot, T_amb_od_step_max)
    compare_des_and_od_at_des(cycle_results)
    return cycle_results
    
def sim_and_save_data__deltaT_UA_config__baseline():
    
    UA_recup_tot_des_par = list(np.arange(10000,61000,10000))       #baseline case
    min_deltaT_par = [0]                # baseline case
    
    file_name = "_baseline_case_results"
                     
    return sim_and_save_data__deltaT_UA_config(min_deltaT_par, UA_recup_tot_des_par, True, file_name)
    
def sim_and_save_data__deltaT_UA_config(deltaT_list, UA_list, is_save_file = True, file_name = "", cycle_config = "", mod_base_dict = {}):
    
    if(cycle_config == ""):
        cycle_config_par = [2,1]
    elif(cycle_config ==  "RC"):
        cycle_config_par = [1]
    elif(cycle_config == "PC"):
        cycle_config_par = [2]
    else:
        return -1
    
    des_results = []
    c_sco2 = C_sco2_sim(1)
    c_sco2.overwrite_des_par_base(mod_base_dict)
    
    for k in range(len(cycle_config_par)):

        cycle_config = cycle_config_par[k]
                                
        if(cycle_config == 1):
            cycle_name = "recompression"
            c_sco2.overwrite_des_par_base_to_recompression()
        elif(cycle_config == 2):
            cycle_name = "partialcooling"
            c_sco2.overwrite_des_par_base_to_partialcooling()
            
        c_sco2.solve_sco2_deltaT_UA_parametrics(deltaT_list, UA_list)
        des_results.append(c_sco2.m_par_solve_dict)
        
        #des_results.append(solve_sco2_deltaT_UA_parametrics(deltaT_list, UA_list, cycle_config))
        
        if(is_save_file):
        
            "Print to json so we can easily read back in for MSPT simulations"
            output = open(cycle_name + file_name + ".txt",'w')
                
            json.dump(des_results[k], output)
            output.close()
            
            "Print to csv so we easily review results"
            with open(cycle_name + file_name + ".csv",'w', newline='') as f:
                w = csv.writer(f)
                w.writerows(des_results[k].items())
            f.close()
        
    return des_results

def plot_sim_and_save_data__deltaT_UA_config__baseline():
    
    UA_recup_tot_des_par = list(np.arange(10000,61000,10000))       #baseline case
    min_deltaT_par = [0,180]                # baseline case
    
    file_name = "_dT_UA_baseline_case_results"
                     
    return plot_sim_and_save_data__deltaT_UA_config(min_deltaT_par, UA_recup_tot_des_par, True, file_name, "RC")

def plot_sim_and_save_data__deltaT_UA_config(deltaT_list, UA_list, is_save_file = True, file_name = "", cycle_config = "", mod_base_dict = {}):
    
    des_results = sim_and_save_data__deltaT_UA_config(deltaT_list, UA_list, is_save_file, file_name, cycle_config, mod_base_dict)
    
    list_des_results = get_list_of_filtered_and_sorted_dicts_from_dict(des_results[0], "min_phx_deltaT", "UA_recup_tot_des")

    str_label_0 = "min_phx_dT_" + str(list_des_results[0]["min_phx_deltaT"][0])
    str_label_1 = "min_phx_dT_" + str(list_des_results[1]["min_phx_deltaT"][0])
    
    if(cycle_config == "RC"):
        plt_cycle_config = "recompression"
    elif(cycle_config == "PC"):
        plt_cycle_config = "partialcooling"
    else:
        plt_cycle_config = "cycle"
    
    c_sco2_out_comp = cy_plt.C_UA_par__stacked_outputs_comp_plot(plt_cycle_config,
                                                                     list_des_results[0],
                                                                     list_des_results[1],
                                                                     str_label_0,
                                                                     str_label_1)
    
    c_sco2_out_comp.create_stacked_cycle_output_comp()
    
    return des_results

def plot_example__eta_vs_UA__deltaT_levels__two_config():
    
    # plots cycle efficiency vs UA at different min_phx_deltaT for both recompression and partialcooling cycles
    
        # Overall RC & PC
    cycle_configs = ["recompression","partialcooling"]
        # Single cycle data
    #cycle_configs = ["recompression"]
    #cycle_configs = ["partialcooling"]
    
    cycle_par_results = [0]
    
    for i_cycle, cycle_config in enumerate(cycle_configs):
        
        dict_i = json.load(open("example_plot_data/" + cycle_config + "_dT_UA_par_sweep_q2_baseline.txt"))
        
        if(i_cycle == 0):
            cycle_par_results[0] = dict_i
        else:
            cycle_par_results.append(dict_i)
    
    cy_plt.plot_eta_vs_UA__deltaT_levels__two_config(cycle_par_results)
    
def plot_and_sim__eta_vs_UA__deltaT_levels__RC_PC(deltaT_list, UA_list):
    
    des_results = [0,0]
    
    des_results[0] = (sim_and_save_data__deltaT_UA_config(deltaT_list, UA_list, is_save_file = False, file_name = "", cycle_config = "RC", mod_base_dict = {}))[0]
    des_results[1] = (sim_and_save_data__deltaT_UA_config(deltaT_list, UA_list, is_save_file = False, file_name = "", cycle_config = "PC", mod_base_dict = {}))[0]
    
    cy_plt.plot_eta_vs_UA__deltaT_levels__two_config(des_results)
    
def plot_example__eta_vs_UA__max_eta_pt__RC_PC():
    
    cycle_config_list = ["recompression","partialcooling"]

    data = [0]
    
    for i_cycle, cycle_config in enumerate(cycle_config_list):
    
        # Load results of min_deltaT -> UA parametric
        data_i = json.load(open("example_plot_data/" + cycle_config + "_dT_UA_par_sweep_q2_baseline.txt"))

        if(i_cycle == 0):
            data[0] = filter_dict_to_list_type_entries_only(data_i)
        else:
            data.append(filter_dict_to_list_type_entries_only(data_i))

    mindT_results = filter_dT_levels_to_mindT_case__eta_band(data, 0.8)
            
    cy_plt.plot_eta_vs_UA__add_UA_saturation_point__multi_configs(mindT_results) 
    
    #Plot from saved data eta vs UA for RC and PC cycle.  Put point on "saturated" UA
 
    
def plot_eg__eta_vs_deltaT__constant_UA__RC_PC():
    
    cycle_config_list = ["recompression","partialcooling"]

    data = [0]
    
    for i_cycle, cycle_config in enumerate(cycle_config_list):
    
        # Load results of min_deltaT -> UA parametric
        data_i = json.load(open("example_plot_data/" + cycle_config + "_dT_UA_par_sweep_q2_baseline.txt"))

        if(i_cycle == 0):
            data[0] = filter_dict_to_list_type_entries_only(data_i)
        else:
            data.append(filter_dict_to_list_type_entries_only(data_i))

    mindT_results = filter_dT_levels_to_mindT_case__eta_band(data, 0.8)

    UA_saturated_list, eta_at_UA_list = cy_plt.calculate_UA_saturated(mindT_results, 0.0025/2, 0.05)
    
    UA_saturated = max(UA_saturated_list)
    
    cy_plt.plot_eta_vs_deltaT__constant_UA__multi_configs(data, UA_saturated)

def sim_and_save_default_new_T_amb__T_amb_od_par(directory, file_name, T_amb_des, T_amb_od_cold, T_amb_od_hot, T_amb_od_step_max = 1):
    
    dict_results = sim_default_new_T_amb__T_amb_od_par(T_amb_des, T_amb_od_cold, T_amb_od_hot, T_amb_od_step_max)
    
    cycle_config = dict_results["cycle_config"]
                                
    if(cycle_config == 1):
        cycle_name = "recompression"
    elif(cycle_config == 2):
        cycle_name = "partialcooling"

    "Print to json"
    output = open(directory + "/" + cycle_name + "_" + file_name + ".txt",'w')
        
    json.dump(dict_results, output)
    output.close()
    
    return dict_results

def sim__T_amb_od__P_mc_in_sweep(T_amb_od, cycle_config = 1):
    
    c_sco2 = C_sco2_sim(cycle_config)
    
    c_sco2.ow_des_par_base__od_P_mc_in_sweep__T_amb_od(T_amb_od)
    
    c_sco2.solve_sco2_case()
    
    return c_sco2.m_solve_dict

def sim_default__P_mc_in_sweep(cycle_config = 1):
    
    c_sco2 = C_sco2_sim(cycle_config)
    
    c_sco2.ow_des_par_base__od_P_mc_in_sweep__default()
    
    c_sco2.solve_sco2_case()
    return c_sco2.m_solve_dict

def sim_default__m_dot_htf_par__T_amb_od_new__T_htf_des(T_amb_od, m_dot_htf_ND_low, m_dot_htf_ND_high, m_dot_htf_ND_step_max = 1):
    
    c_sco2 = C_sco2_sim(1)
    
    # Find cold side delta_m_dot that is equal to or less than max step
    n_m_dot_low = np.ceil((1.0 - m_dot_htf_ND_low) / m_dot_htf_ND_step_max)
    delta_m_dot = (1.0 - m_dot_htf_ND_low) / n_m_dot_low
    od_low = list(np.arange(m_dot_htf_ND_low, 1.0, delta_m_dot))
    
    # Find hot side delta_m_dot that is equal to or less than max step
    if(m_dot_htf_ND_high == 1.0):
        od_high = [1.0]
    else:
        n_m_dot_high = np.ceil((m_dot_htf_ND_high - 1.0) / m_dot_htf_ND_step_max)
        delta_m_dot = (m_dot_htf_ND_high - 1.0) / n_m_dot_high
        od_high = list(np.arange(1.0, m_dot_htf_ND_high*1.0001, delta_m_dot))
    
    list_od_m_dots = od_low + od_high
    print(list_od_m_dots)
    
    c_sco2.ow_des_par_base__m_dot_htf_par__T_amb_od__T_htf_des(list_od_m_dots, T_amb_od)
    
    c_sco2.solve_sco2_case()
    return c_sco2.m_solve_dict

def sim_default_new_T_amb__T_amb_od_par(T_amb_des, T_amb_od_cold, T_amb_od_hot, T_amb_od_step_max = 1):

    c_sco2 = C_sco2_sim(1)       # Recompression cycle default
    
    # Overwrite des par base with 'T_amb_des'
    mod_base_dict = {"T_amb_des" : T_amb_des}
    c_sco2.overwrite_des_par_base(mod_base_dict)

    # Find cold side delta_T that is equal to or less than max step
    n_cold = np.ceil((T_amb_des - T_amb_od_cold) / T_amb_od_step_max)
    delta_T_cold = (T_amb_des - T_amb_od_cold) / n_cold
    od_cold = list(np.arange(T_amb_od_cold, T_amb_des, delta_T_cold))

    # Find hot side delta_T that is equal to or less than max step
    n_hot = np.ceil((T_amb_od_hot - T_amb_des) / T_amb_od_step_max)
    delta_T_hot = (T_amb_od_hot - T_amb_des) / n_hot          
    od_hot = list(np.arange(T_amb_des, T_amb_od_hot + 0.01, delta_T_hot))
    
    list_od_temps = od_cold + od_hot
    print(list_od_temps)
    
    c_sco2.ow_des_par_base_T_amb_od_par_m_dot_T_htf_des(list_od_temps)
    
    c_sco2.solve_sco2_case()
    return c_sco2.m_solve_dict

def get_one_des_dict_from_par_des_dict(par_dict, par_key, index):
    
    n_len = len(par_dict[par_key])
    
    return cy_plt.filter_dict_to_index(cy_plt.filter_dict_column_length(par_dict, n_len), index)

def write_dictionary_to_csv(dict_in, csv_write_obj):
    
    for key in dict_in:
        row = [key]
        if(isinstance(dict_in[key],list)):        
            for val in dict_in[key]:
                row.append(val)
        else:
            row.append(dict_in[key])
        csv_write_obj.writerow(row)

def compare_des_and_od_at_des(dict_results, i_od = 0):
    
    data_length, data_type = get_entry_data_type(dict_results["eta_thermal_calc"])
    
    if(data_length != "single"):
        print("method 'compare_des_and_od_at_des' expects a single design point")
        
    data_length, data_type = get_entry_data_type(dict_results["T_amb_od"])
    if(data_length != "list"):
        print("method 'compare_des_and_od_at_des' expects a list of at least length 1 off-design points")
    
    n_od = len(dict_results["T_amb_od"])
    if(n_od < 1):
        print("method 'compar_des_and_od_at_des' requires a list of at least length 1 off-design points")
    
    i_od_in = i_od
    if(i_od < 0):
        i_od = 0
        print("Using", i_od, "for off-design list index. Input index was", i_od_in)
        
    if(i_od > n_od - 1):
        i_od = n_od - 1
        print("Using", i_od, "for off-design list index. Input index was", i_od_in)
    
    print("Independent Inputs:")
    dict_info = get_des_od_label_unit_info__ind_inputs()
    for i, key in enumerate(dict_info.keys()):
        dict_info[key].print_des_od_comp(dict_results, i_od)

    print("\nCalculated Metrics:")
    dict_info = get_des_od_label_unit_info__calc_metrics()
    for i, key in enumerate(dict_info.keys()):
        dict_info[key].print_des_od_comp(dict_results, i_od)
    
    

def filter_dT_levels_to_mindT_case__eta_band(list_des_results, eta_frac):

    plot_data_min_mindT = [0]

    for i_cycle, val_unused in enumerate(list_des_results):

        # Sort remaining data by outer next variable: min_phx_deltaT
        min_mindT = min(list_des_results[i_cycle]["min_phx_deltaT"])
        max_eta = max(list_des_results[i_cycle]["eta_thermal_calc"])
        
        # Filter to contain only data with 'min_phx_deltaT' = min_mindT
        plot_data_mindT_eta_floor_i = []
        
        for i, val in enumerate(list_des_results[i_cycle]["min_phx_deltaT"]):
            if val == min_mindT:
                # Filter for data min dT
                if (list_des_results[i_cycle]["eta_thermal_calc"][i] > eta_frac*max_eta):
                    # Filter out low efficiencies so y-axis is more compact - this gets indices
                    plot_data_mindT_eta_floor_i.append(i)
        
        # This generates new data set from filtered indices
        mindT_filtered = dict((key, [val[i] for i in plot_data_mindT_eta_floor_i]) for key, val in list_des_results[i_cycle].items())
        if(i_cycle == 0):
            plot_data_min_mindT[0] = mindT_filtered
        else:
            plot_data_min_mindT.append(mindT_filtered)
            
    return plot_data_min_mindT


def get_list_of_filtered_and_sorted_dicts_from_dict(dict_in, str_filtered, str_sorted):
    
    #filter to only include list entries so we can move into pandas
    dict_lists = filter_dict_to_list_type_entries_only(dict_in)
    
    #get a set of unique 'str_filtered' values
    filtered_set = set(dict_lists[str_filtered])
    
    #move into pandas data frame
    df = pd.DataFrame.from_dict(dict_lists)
    
    #get a list of dictionaries, one item for each unique min_phx_deltaT
    #sort each dictionary by increasing 'ua_recup_tot_des"
    list_of_dfs = [0]
    
    for i, temp in enumerate(filtered_set):
        
        df_local = df.loc[df[str_filtered] == temp]
        df_local = df_local.sort_values(by=[str_sorted])
        dict_local = df_local.to_dict('list')
        
        if(i==0):
            list_of_dfs[0] = dict_local
            
        else:
            list_of_dfs.append(dict_local)
            
    return list_of_dfs

def combine_dicts_with_same_items(dict1, dict2):
    d = dict1.copy()
    for key, value in d.items():
        value.extend(dict2[key])
        
    return d

def filter_dict_to_list_type_entries_only(dict_in):

    dict_out = {}
    
    for key in dict_in:
        
        length_type, data_type = get_entry_data_type(dict_in[key])
        
        if( length_type == "matrix" or length_type =="list"):
            
            dict_out[key] = dict_in[key]
            
    return dict_out

def compare_two_json_files(json_in_1, desc_1, json_in_2, desc_2, comp_txt_out):

    dict_1 = json.load(open(json_in_1))
    dict_2 = json.load(open(json_in_2))

    comp_str = compare_two_dict_by_keys(dict_1, desc_1, dict_2, desc_2)

    outfile = open(comp_txt_out, "w")
    outfile.write(comp_str)
    outfile.close()

def compare_two_dict_by_keys(dict_1, desc_1, dict_2, desc_2):

    d1_keys = set(dict_1.keys())
    d2_keys = set(dict_2.keys())

    intersect_keys = d1_keys.intersection(d2_keys)

    added_keys_d1 = d1_keys - d2_keys
    local_str = "added keys " + desc_1 + " = " + str(added_keys_d1)
    print(local_str)
    key_summary_str = local_str + " \n"
    local_str = "length added keys " + desc_1 + " = " + str(len(added_keys_d1))
    print(local_str)
    key_summary_str = key_summary_str + local_str + " \n"

    added_keys_d2 = d2_keys - d1_keys
    local_str = "added keys " + desc_2 + " = " + str(added_keys_d2)
    print(local_str)
    key_summary_str = key_summary_str + local_str + " \n"
    local_str = "length added keys " + desc_2 + " = " + str(len(added_keys_d2))
    print(local_str)
    key_summary_str = key_summary_str + local_str + " \n"

    mod_keys_set = set(o for o in intersect_keys if dict_1[o] != dict_2[o])
    modified_keys = list(mod_keys_set)
    local_str = "modified keys = " + str(mod_keys_set)
    print(local_str)
    key_summary_str = key_summary_str + local_str + " \n"

    mismatch_str = ""
    match_str = ""

    # i_key = modified_keys[0]
    for i_key in modified_keys:

        i_data1_key = dict_1[i_key]
        i_data2_key = dict_2[i_key]

        c1_data = C_data_properties(i_data1_key, desc_1)
        c2_data = C_data_properties(i_data2_key, desc_2)

        mismatch_str_local, match_str_local = compare_data_properties(c1_data, c2_data)

        key_str = "For key " + str(i_key)

        if (mismatch_str_local != ""):
            mismatch_str = mismatch_str + key_str + " \n" + mismatch_str_local + " \n"
        elif (match_str_local != ""):
            match_str = match_str + key_str + " \n" + match_str_local + " \n"
        else:
            print("both strings null")

    print("match str = ", match_str)
    print("mismatch str = ", mismatch_str)

    return key_summary_str + match_str + mismatch_str

class C_data_properties:

    def __init__(self, data, name = ""):
        self.data = data

        self.name = name

        self.structure_type, self.data_type, self.l_d1, self.l_d2 = get_entry_data_properties(self.data)

def get_np_list_statistics(np_list):

    min_out = min(np_list)
    max_out = max(np_list)
    max_abs_out = max(abs(min_out), abs(max_out))

    mean = np.mean(np_list)
    rms = np.sqrt(np.mean(np_list**2))
    mean_abs_error = np.mean(np.abs(np_list))

    out_str = "The differences are " + str(np_list) + "\n" + \
        "The maximum absolute difference is " + str(max_abs_out) + "\n" + \
        "The mean difference is " + str(mean) + "\n" + \
        "The RMS difference is " + str(rms) + "\n" + \
        "The mean absolute difference is " + str(mean_abs_error) + "\n"

    return max_abs_out, mean, rms, mean_abs_error, out_str


def compare_data_properties(c_data_1, c_data_2):

    mismatch_str = ""
    match_str = ""

    value_str = "The " + c_data_1.name + " data unit value is = " + str(c_data_1.data) + "\n" +\
                "The " + c_data_2.name + " data unit value is = " + str(c_data_2.data) + "\n"

    if(c_data_1.structure_type == c_data_2.structure_type):

        if(c_data_1.data_type != c_data_2.data_type):

            mismatch_str = "Both data units have the same structure, but " + \
                    "the " + c_data_1.name + " data unit is type " + c_data_1.data_type + \
                    ", but the " + c_data_2.name + " data unit is type " + c_data_2.data_type + "\n"
            mismatch_str = mismatch_str + value_str

        else:

            if(c_data_1.data_type == "float"):

                if(c_data_1.structure_type == "single"):

                    if(c_data_1.data != 0):
                        perc_diff = (c_data_2.data - c_data_1.data) / c_data_1.data * 100.0
                        match_str = "The relative difference between the " + \
                                    c_data_1.name + " data unit and the " + c_data_2.name + " data unit " +\
                                    " is " + str(perc_diff) + " %\n"
                    else:
                        data_diff = (c_data_2.data - c_data_1.data)
                        match_str = "The difference between the " + \
                                    c_data_2.name + " data unit and the " + c_data_1.name + " data unit " + \
                                    " is " + str(data_diff) + "\n"

                    match_str = match_str + value_str

                elif(c_data_1.structure_type == "list"):

                    if(c_data_1.l_d1 != c_data_2.l_d1):
                        mismatch_str = "The " + c_data_1.name + " data unit is a list of length " + \
                                       str(c_data_1.l_d1) + ", but the " + \
                                       c_data_2.name + " data unit is a list of length " + str(c_data_2.l_d1) + "\n"
                        mismatch_str = mismatch_str + value_str

                    else:
                        list_diff = np.array(c_data_2.data) - np.array(c_data_1.data)

                        match_str = "Statistics on the difference between the " + \
                                    c_data_2.name + " data unit and the " + c_data_1.name + " data unit: \n"

                        max_abs_calc, mean_calc, rms_calc, mean_abs_error_calc, out_str_calc = get_np_list_statistics(list_diff)

                        match_str = match_str + out_str_calc + value_str

                elif(c_data_1.structure_type == "matrix"):

                    if(c_data_1.l_d1 != c_data_2.l_d2 or c_data_1.l_d2 != c_data_2.l_d2):
                        mismatch_str = "The " + c_data_1.name + " data unit is a matrix " + \
                                       ", with at least dimension that is a different length than matrix " + \
                                       c_data_2.name
                        mismatch_str = mismatch_str + value_str

                    else:
                        np1 = np.array(c_data_1.data)
                        np2 = np.array(c_data_2.data)

                        flatten_list_diff = np2.flatten() - np1.flatten()

                        match_str = "Statistics on the difference between the " + \
                                    c_data_2.name + " data unit and the " + c_data_1.name + " data unit: \n"

                        max_abs_calc, mean_calc, rms_calc, mean_abs_error_calc, out_str_calc = get_np_list_statistics(
                            flatten_list_diff)

                        match_str = match_str + out_str_calc + value_str

                else:
                    mismatch_str = "Structure type " + c_data_2.structure_type + " can't be further processed\n"
                    mismatch_str = mismatch_str + value_str

            else:

                match_str = "Both data units have the same structure and data types, " + \
                            "but the data type is not float so there is no statistical analysis\n"
                match_str = match_str + value_str

    else:

        mismatch_str = "The " + c_data_1.name + " data unit is structured as a " + \
               c_data_1.structure_type + ", but the " + \
               c_data_2.name + " data unit is structured as a " + c_data_2.structure_type + "\n"

        mismatch_str = mismatch_str + value_str

    return mismatch_str, match_str


def test_get_entry_data_properties():
    
    s1_data = [[1.1, 1.2, 1.3],[2.1, 2.2, 2.3]]
    #s1_data = [[[11.1, 11.2, 11.3], [22.1, 22.2, 22.3]], [[11.1, 11.2, 11.3], [22.1, 22.2, 22.3]]]
    #s1_data = [1.1, 2.2, 3.3]
    #s1_data = 'hello world'
    #s1_data = 0
    s2_data = [[11.1, 11.2, 11.3], [22.1, 22.2, 22.3]]
    #s2_data = [[[11.1, 11.2, 11.3], [22.1, 22.2, 22.3]],[[11.1, 11.2, 11.3], [22.1, 22.2, 22.3]]]
    #s2_data = 'hello world'
    #s2_data = [11.1, 22.2, 33.3]
    #s2_data = 3
    
    c1_data = C_data_properties(s1_data, "first")
    c2_data = C_data_properties(s2_data, "second")

    #print("structure type = ", c2_data.structure_type)
    #print("data type = ", c2_data.data_type)
    #print("list dimension = ", c2_data.l_d1)
    #print("matrix dimension = ", c2_data.l_d2)

    mismatch_str, match_str = compare_data_properties(c1_data, c2_data)

    print("mismatch string = \n", mismatch_str)
    print("match string = \n", match_str)

def get_entry_data_properties(data_in):
    structure_type = "undefined"
    data_type = "undefined"
    l_d1 = -1
    l_d2 = -1

    # is 'data_in' a list?
    if (isinstance(data_in, list)):

        # is the first entry in the list 'data_in' also a list?
        if (isinstance(data_in[0], list)):

            # is the first entry in the first row of the matrix 'data_in' also a list?
            if(isinstance(data_in[0][0], list)):
                structure_type = "array_of_3_or_more_dimensions"
                data_type = "unknown"

            else:

                structure_type = "matrix"
                
                for k in range(len(data_in)):

                    if(k==0):
                        k_0_structure_type, k_0_data_type, k_0_l_d1, k_0_l_d2 = get_list_data_type(data_in[k])
                        k_structure_type = k_0_structure_type
                        is_matrix_same_data_type = True
                        are_lists_same_length = True
                    else:
                        k_structure_type, k_data_type, k_l_d1, k_l_d2 = get_list_data_type(data_in[k])
                        if(k_0_data_type != k_data_type):
                            is_matrix_same_data_type = False 
                        if(k_0_l_d1 != k_l_d1):
                            are_lists_same_length = False
                            
                        if(k_structure_type != "list"):
                            structure_type = "matrix_of_lists_and_others"

                if(len(data_in) == 1):
                    k_l_d1 = k_0_l_d1

                if(is_matrix_same_data_type):
                    data_type = k_0_data_type
                else:
                    data_type = "other"
                    
                if(not(are_lists_same_length)):
                    structure_type = "matrix_of_unequal_list_length"
                    
                return structure_type, data_type, len(data_in), k_l_d1


        else:

            return get_list_data_type(data_in)

    else:
        structure_type = "single"
        data_type = get_single_value_data_type(data_in)
        l_d1 = -1
        l_d2 = -1

    return structure_type, data_type, l_d1, l_d2

def get_list_data_type(val_in):

    j_0_data_type = "unknown"
    for j in range(len(val_in)):

        if (j == 0):
            structure_type = "list"
            j_0_data_type = get_single_value_data_type(val_in[j])
            j_data_type = j_0_data_type
            is_list_same_data_type = True
            l_d1 = len(val_in)
            l_d2 = -1
        else:
            j_data_type = get_single_value_data_type(val_in[j])
            if (j_0_data_type != j_data_type):
                is_list_same_data_type = False

        if (j_data_type == "input_value_is_list"):
            structure_type = "list_of_singles_and_others"

    if (is_list_same_data_type):
        data_type = j_0_data_type
    else:
        data_type = "other"

    return structure_type, data_type, l_d1, l_d2


def get_single_value_data_type(val_in):

    if (isinstance(val_in, list)):
        return "input_value_is_list"
    elif (isinstance(val_in, str)):
        return "string"
    elif (math.isnan(float(val_in))):
        return "nan"
    elif (str(val_in) == "nan"):
        return "string_nan"
    elif (isinstance(val_in, float)):
        return "float"
    elif (isinstance(val_in, int)):
        return "float"
    else:
        return "unknown"


def get_entry_data_type(data_in):
    
    data_type = "undefined"
    length_type = "undefined"
    
    if(isinstance(data_in,list)):
        for j in range(len(data_in)):

            if(isinstance(data_in[j],list)):                
                length_type = "matrix"
                data_type = "float"
#                for k in range(len(data_in[j])):
#                    if(isinstance(data_in[j][k],float)):
#                        print("fffloat: ", str(key) + str(baseline[key][i][j][k]))
#                    elif(isinstance(baseline[key][i][j][k],int)):
#                        print("iiint: ", str(key) + str(baseline[key][i][j][k]))
#                    elif(isinstance(baseline[key][i][j][k],str)):
#                        print("ssstring: ", str(key) + str(baseline[key][i][j][k]))
#                    else:
#                        print("eeelse: ", str(key) + str(baseline[key][i][j][k]))
            elif(isinstance(data_in[j],str)):
                length_type = "list"
                data_type = "string"
            elif(math.isnan(float(data_in[j]))):
                length_type = "list"
                data_type = "nan"
            elif(str(data_in[j]) == "nan"):
                length_type = "list"
                data_type = "str_nan"
                #print("Str_list1: ", str(key) + " " + str(baseline[key][i][j]))
            elif(isinstance(data_in[j],float)):
                length_type = "list"
                data_type = "float"
            elif(isinstance(data_in[j],int)):
                length_type = "list"
                data_type = "float"
            else:
                length_type = "list"
                data_type = "other"
                    
    elif(isinstance(data_in,str)):
        length_type = "single"
        data_type = "string"
    elif(math.isnan(float(data_in))):
        length_type = "single"
        data_type = "nan"
    elif(str(data_in)=="nan"):
        length_type = "single"
        data_type = "string_nan"
    elif(isinstance(data_in,float)):
        length_type = "single"
        data_type = "float"
    elif(isinstance(data_in,int)):
        length_type = "single"
        data_type = "float"
    elif(isinstance(data_in,str)):
        length_type = "single"
        data_type = "string"
    else:
        length_type = "single"
        data_type = "other"
        
    return length_type, data_type


def compare_dict_files(baseline, mod):

    # nan is not a string, "nan" is a string...
    
    print("\nComparing results ... \n")
    l_type, d_type = get_entry_data_type(baseline["cycle_config"])
    
    if(l_type == "list"):
        base_cycle_config = baseline["cycle_config"][0]
        mod_cycle_config = baseline["cycle_config"][0]
    else:
        base_cycle_config = baseline["cycle_config"]
        mod_cycle_config = baseline["cycle_config"]
    
    if(base_cycle_config != mod_cycle_config):
        print("The baseline cycle configuration is the " + base_cycle_config + " but the modified cycle configuration is the " + mod_cycle_config)
        return
    
    if(base_cycle_config == 1):
        s_base_cycle_config = "RC"
    elif(base_cycle_config == 2):
        s_base_cycle_config = "PC"
    else:
        s_base_cycle_config = "undefined cycle"
    
    for key in baseline:
        if key in mod:
            
            l_type, d_type = get_entry_data_type(baseline[key])
            if(l_type == "single"):
                baseline[key] = [baseline[key]]
                
            l_type, d_type = get_entry_data_type(mod[key])
            if(l_type == "single"):
                mod[key] = [mod[key]]
            
            n_key_baseline = len(baseline[key])
            n_key_mod = len(mod[key])
            
            if(n_key_baseline != n_key_mod):
                print(s_base_cycle_config + " For "+str(key)+" the baseline case has "+str(n_key_baseline)+" entries and the modified case has "+str(n_key_mod)+" entries")
                
            n_compare = min(n_key_baseline, n_key_mod)
            
            for i in range(n_compare):
                
                # Get 'length_type' and 'data_type' of baseline
                l_type_baseline, d_type_baseline = get_entry_data_type(baseline[key][i])
                # Get 'length_type' and 'data_type' of mod
                l_type_mod, d_type_mod = get_entry_data_type(mod[key][i])
                
                if(l_type_baseline != l_type_mod or d_type_baseline != d_type_mod):
                    o_str = s_base_cycle_config + " For key = "+str(key)+" run number "+str(i)+ "\n"
                    o_str = o_str + "The baseline type is "+d_type_baseline+" and the value is "+str(baseline[key][i]) + "\n"
                    o_str = o_str + "The modified type is "+d_type_mod+" and the value is "+str(mod[key][i]) + "\n\n"
                    print(o_str)
                    
                elif(l_type_baseline == "single"): 
                    if(baseline[key][i] != mod[key][i] and d_type_baseline != "nan"):
                        o_str = s_base_cycle_config + " For key = "+str(key)+" run number "+str(i) + " of data type " + d_type_baseline + "\n"
                        o_str = o_str + "The baseline value is " + str(baseline[key][i]) + "\n"
                        o_str = o_str + "The modified value is " + str(mod[key][i]) + "\n\n"
                        print(o_str)
                        
                else:
                    
                    n_a_base = len(baseline[key][i])
                    n_a_mod = len(mod[key][i])
                    
                    n_a_comp = min(n_a_base, n_a_mod)
                    
                    if(n_a_base != n_a_mod):
                        o_str = s_base_cycle_config + " For "+str(key)+" run number "+str(i)+" index\n"
                        o_str = o_str + "The baseline case has "+str(n_a_base)+ "entries\n"
                        o_str = o_str + "The modified case has "+str(n_a_mod)+" entries\n"
                        o_str = o_str + "The comparison code will only evaluate the first "+str(n_a_comp)+" values."
                        print(o_str)
                    
                    for j in range(n_a_comp):
                    
                        l_type_baseline, d_type_baseline = get_entry_data_type(baseline[key][i][j])
                        l_type_mod, d_type_mod = get_entry_data_type(mod[key][i][j])
                        
                        if(l_type_baseline != l_type_mod or d_type_baseline != d_type_mod):
                            o_str = s_base_cycle_config + " For key = "+str(key)+" run number "+str(i)+" index "+str(j)+ "\n"
                            o_str = o_str + "The baseline type is "+d_type_baseline+" and the value is "+str(baseline[key][i][j]) + "\n"
                            o_str = o_str + "The modified type is "+d_type_mod+" and the value is "+str(mod[key][i][j]) + "\n\n"
                            print(o_str)
                    
                        elif(l_type_baseline == "single"): 
                            if(baseline[key][i][j] != mod[key][i][j] and d_type_baseline != "nan"):
                                o_str = s_base_cycle_config + " For key = "+str(key)+" run number "+str(i)+" index "+str(j)+ " of data type " + d_type_baseline + "\n"
                                o_str = o_str + "The baseline value is " + str(baseline[key][i][j]) + "\n"
                                o_str = o_str + "The modified value is " + str(mod[key][i][j]) + "\n\n"
                                print(o_str)
                                
                        elif(baseline[key][i][j] != mod[key][i][j] and d_type_baseline != "nan"):
                            o_str = s_base_cycle_config + " For key = "+str(key)+" run number "+str(i)+" index "+str(j)+ " of data type " + d_type_baseline + "\n"
                            o_str = o_str + "The baseline value is " + str(baseline[key][i][j]) + "\n"
                            o_str = o_str + "The modified value is " + str(mod[key][i][j]) + "\n\n"
                            print(o_str)


 
    





