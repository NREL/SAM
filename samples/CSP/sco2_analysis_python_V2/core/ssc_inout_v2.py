# -*- coding: utf-8 -*-
"""
Created on Fri Jun  9 10:56:12 2017

@author: tneises
"""
import string

import PySSC as sscapi

def ssc_get_dll():
    ssc = sscapi.PySSC()
    return ssc.get_dll()

def ssc_cmod(dat, name, is_ssc_print = True):
    ssc = sscapi.PySSC()
    cmod = ssc.module_create(name.encode("utf-8"))
    int_print = int(is_ssc_print)
    ssc.module_exec_set_print(int_print)
                             
    # Run compute module
    # Check for simulation errors
    if ssc.module_exec(cmod, dat) == 0:
        print (name + ' simulation error')
        idx = 1
        msg = ssc.module_log(cmod, 0)
        while (msg != None):
            print (' : ' + msg.decode("utf - 8"))
            msg = ssc.module_log(cmod, idx)
            idx = idx + 1
        cmod_err_dict = ssc_table_to_dict(cmod, dat)
        return [False, cmod_err_dict]

    # Get python dictionary representing compute module with all inputs/outputs defined
    cmod_dict = ssc_table_to_dict(cmod, dat)
    sscapi.PySSC().module_free(cmod)
    return [True, cmod_dict]
    
def cmod_sco2_udpc(dat_dict):
    
    cmod_name = "sco2_csp_ud_pc_tables"
    dat = dict_to_ssc_table(dat_dict, cmod_name)
    val = ssc_cmod(dat, cmod_name)
    sscapi.PySSC().data_free(dat)
    return val

def cmod_sco2_csp_system(dat_dict):
    
    cmod_name = "sco2_csp_system"
    dat = dict_to_ssc_table(dat_dict, cmod_name)
    val = ssc_cmod(dat, cmod_name)
    sscapi.PySSC().data_free(dat)
    return val

def cmod_sco2_air_cooler(dat_dict):

    cmod_name = "sco2_air_cooler"
    dat = dict_to_ssc_table(dat_dict, cmod_name)
    val = ssc_cmod(dat, cmod_name)
    sscapi.PySSC().data_free(dat)
    return val

def cmod_mspt_from_dict(dat_dict, is_SO_financial = True, is_ssc_print = True):
    
    mspt_name = "tcsmolten_salt"
    # Convert python dictionary into ssc var info table
    dat = dict_to_ssc_table(dat_dict, mspt_name)
    
    so_name = "singleowner"
    dat = dict_to_ssc_table_dat(dat_dict, so_name, dat)

    val = cmod_mspt(dat, is_SO_financial, is_ssc_print)
    sscapi.PySSC().data_free(dat)

    return val

def cmod_mspt(dat, is_SO_financial = True, is_ssc_print = True):
    
    # Run the molten salt power tower compute module
    mspt_name = "tcsmolten_salt"
    mspt_return = ssc_cmod(dat, mspt_name, is_ssc_print)
    mspt_success = mspt_return[0]
    mspt_dict = mspt_return[1]
    
    if(mspt_success == 0):
        mspt_dict["cmod_success"] = 0
        return mspt_dict
    
    if(is_SO_financial == False):
        mspt_dict["cmod_success"] = 1
        return mspt_dict

    # Run the single owner financial model
    cmod_name = "singleowner"
    cmod_return = ssc_cmod(dat, cmod_name)
    cmod_success = cmod_return[0]
    so_dict = cmod_return[1]
    
    if(cmod_success == 0):
        so_dict["cmod_success"] = 0
        out_err_dict = mspt_dict.copy()
        return out_err_dict.update(so_dict)
    
    # If all models successful, set boolean true
    so_dict["cmod_success"] = 1
    
    # Combine mspt and single owner dictionaries
    out_dict = mspt_dict.copy()
    out_dict.update(so_dict)

    return out_dict

def cmod_generic_from_dict(dat_dict, is_SO_financial = True):
    
    mspt_name = "generic_system"
    # Convert python dictionary into ssc var info table
    dat = dict_to_ssc_table(dat_dict, mspt_name)
    
    so_name = "singleowner"
    dat = dict_to_ssc_table_dat(dat_dict, so_name, dat)

    val = cmod_generic(dat, is_SO_financial)
    sscapi.PySSC().data_free(dat)

    # dat is set to dat_mspt in cmod_mspt, so only need to set one?
    #sscapi.PySSC().data_free(dat_mspt)

    return val

def cmod_generic(dat, is_SO_financial = True):
    
    # Run the molten salt power tower compute module
    tech_name = "generic_system"
    tech_return = ssc_cmod(dat, tech_name)
    tech_success = tech_return[0]
    tech_dict = tech_return[1]
    
    if(tech_success == 0):
        tech_dict["cmod_success"] = 0
        return tech_dict
    
    if(is_SO_financial == False):
        tech_dict["cmod_success"] = 1
        return tech_dict

    # Run the single owner financial model
    cmod_name = "singleowner"
    cmod_return = ssc_cmod(dat, cmod_name)
    cmod_success = cmod_return[0]
    so_dict = cmod_return[1]
    
    if(cmod_success == 0):
        so_dict["cmod_success"] = 0;
        out_err_dict = tech_dict.copy()
        return out_err_dict.update(so_dict)
    
    # If all models successful, set boolean true
    so_dict["cmod_success"] = 1
    
    # Combine mspt and single owner dictionaries
    out_dict = tech_dict.copy()
    out_dict.update(so_dict)

    return out_dict
   
def mspt_so_ssc_table_to_dict(dat):
    
    # Input the mspt var input table
    # Return that information in a python dictionary
    ssc = sscapi.PySSC()
    mspt_name = "tcsmolten_salt"
    cmod = ssc.module_create(mspt_name.encode("utf-8"))
    
    mspt_dict = ssc_table_to_dict(cmod, dat)
    
    so_name = "singleowner"
    so_cmod = ssc.module_create(so_name.encode("utf-8"))
    
    so_dict = ssc_table_to_dict(so_cmod, dat)
    
    out_dict = mspt_dict.copy()
    
    out_dict.update(so_dict)

    sscapi.PySSC().module_free(cmod)
    sscapi.PySSC().module_free(so_cmod)
    
    return out_dict
    

# Returns python dictionary representing SSC compute module w/ all required inputs/outputs defined
def ssc_table_to_dict(cmod, dat):
    ssc = sscapi.PySSC()
    i = 0
    ssc_out = {}
    while (True):
        p_ssc_entry = ssc.module_var_info(cmod,i)
        ssc_output_data_type = ssc.info_data_type(p_ssc_entry)
        if (ssc_output_data_type <= 0 or ssc_output_data_type > 5):
            break
        ssc_output_data_name = str(ssc.info_name(p_ssc_entry).decode("ascii"))
        ssc_data_query = ssc.data_query(dat, ssc_output_data_name.encode("ascii"))
        if(ssc_data_query > 0):            
            if(ssc_output_data_type == 1):
                ssc_out[ssc_output_data_name] = ssc.data_get_string(dat, ssc_output_data_name.encode("ascii")).decode("ascii")
            elif(ssc_output_data_type == 2):
                ssc_out[ssc_output_data_name] = ssc.data_get_number(dat, ssc_output_data_name.encode("ascii"))
            elif(ssc_output_data_type == 3):
                ssc_out[ssc_output_data_name] = ssc.data_get_array(dat, ssc_output_data_name.encode("ascii"))
            elif(ssc_output_data_type == 4):
                ssc_out[ssc_output_data_name] = ssc.data_get_matrix(dat, ssc_output_data_name.encode("ascii"))
            elif(ssc_output_data_type == 5):
                ssc_out[ssc_output_data_name] = ssc.data_get_table(dat, ssc_output_data_name.encode("ascii"))
        i = i+1
    
    return ssc_out

# Returns python dictionary with empty lists for each SSC NUMBER input/output
def ssc_table_numbers_to_dict_empty(cmod_name):
    ssc = sscapi.PySSC()
    cmod = ssc.module_create(cmod_name.encode("utf-8"))
    i = 0
    ssc_out = {}
    while (True):

        p_ssc_entry = ssc.module_var_info(cmod,i)
        
        ssc_output_data_type = ssc.info_data_type(p_ssc_entry)
        
        #1 = String, 2 = Number, 3 = Array, 4 = Matrix, 5 = Table
        if (ssc_output_data_type <= 0 or ssc_output_data_type > 5):
            break
           
        ssc_output_data_name = str(ssc.info_name(p_ssc_entry).decode("ascii"))
         
        if(ssc_output_data_type == 1):
            ssc_out[ssc_output_data_name] = []
        elif(ssc_output_data_type == 2):
            ssc_out[ssc_output_data_name] = []
        elif(ssc_output_data_type == 3):
            ssc_out[ssc_output_data_name] = []
        elif(ssc_output_data_type == 4):
            ssc_out[ssc_output_data_name] = []
        elif(ssc_output_data_type == 5):
            ssc_out[ssc_output_data_name] = []
        i += 1
    sscapi.PySSC().module_free(cmod)
    return ssc_out
        

def dict_to_ssc_table(py_dict, cmod_name):
    ssc = sscapi.PySSC()
    dat = ssc.data_create()
    return dict_to_ssc_table_dat(py_dict, cmod_name, dat)

def dict_to_ssc_table_dat(py_dict, cmod_name, dat):
    ssc = sscapi.PySSC()
    
    cmod = ssc.module_create(cmod_name.encode("utf-8"))
    
    dict_keys = list(py_dict.keys())
    #dat = ssc.data_create()
                         
    ii = 0
    while (True):
        
        p_ssc_entry = ssc.module_var_info(cmod,ii)
        
        ssc_input_data_type = ssc.info_data_type(p_ssc_entry)
            
        # 1 = String, 2 = Number, 3 = Array, 4 = Matrix, 5 = Table
        if (ssc_input_data_type <= 0 or ssc_input_data_type > 5):
            break
        
        ssc_input_var_type = ssc.info_var_type(p_ssc_entry)
        
        # If the variable type is INPUT (1) or INOUT (3)
        if(ssc_input_var_type == 1 or ssc_input_var_type == 3):
    
            # Get name of iith variable in compute module table
            ssc_input_data_name = str(ssc.info_name(p_ssc_entry).decode("ascii"))
            
            # Find corresponding 'des_par' dictionary item
            is_str_test_key = False
            for i in range(len(dict_keys)):
                if(dict_keys[i] == ssc_input_data_name):
                    is_str_test_key = True
                    #print ("Found key")
                    break
            
            # Good debug code
            #if(is_str_test_key == False):
            #    print ("Did not find key: ", ssc_input_data_name)
            
            # Set compute module data to dictionary value
            if (is_str_test_key == True):
                if(ssc_input_data_type == 1):
                    ssc.data_set_string(dat, ssc_input_data_name.encode("ascii"), py_dict[ssc_input_data_name].encode("ascii"))
                elif(ssc_input_data_type == 2):
                    ssc.data_set_number(dat, ssc_input_data_name.encode("ascii"), py_dict[ssc_input_data_name])
                elif(ssc_input_data_type == 3):
                    ssc.data_set_array(dat, ssc_input_data_name.encode("ascii"), py_dict[ssc_input_data_name])
                elif(ssc_input_data_type == 4):
                    ssc.data_set_matrix(dat, ssc_input_data_name.encode("ascii"), py_dict[ssc_input_data_name])
                elif(ssc_input_data_type == 5):
                    ssc.data_set_table(dat, ssc_input_data_name.encode("ascii"), py_dict[ssc_input_data_name])
        
        ii = ii+1
        
    sscapi.PySSC().module_free(cmod)

    return dat
        
def str_from_dict(py_dict):
    
    dict_keys = list(py_dict.keys())
    
    ci = 0
    long_file_name = ""
                    
    for i in dict_keys:
            
            if(ci <= 25):
                long_file_name = long_file_name + string.ascii_lowercase[ci] + str(py_dict[i]) + "_"
            elif(ci <= 51):
                long_file_name = long_file_name + "a" + string.ascii_lowercase[ci-26] + str(py_dict[i]) + "_"
            else:
                long_file_name = long_file_name + "aa" + string.ascii_lowercase[ci-52] + str(py_dict[i]) + "_"
            ci = ci + 1
    
    return long_file_name
        