import csv
import numpy as np
import pandas as pd
import sys
import os
import matplotlib.pyplot as plt
import threading as thrd

from matplotlib import cm
from matplotlib.ticker import LinearLocator
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.lines import Line2D
from matplotlib.widgets import Button
from matplotlib.backend_bases import MouseButton
import matplotlib
matplotlib.use('Qt5Agg')
from matplotlib.backends import qt_compat

parentDir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(parentDir)

newPath = os.path.join(parentDir, 'core')
sys.path.append(newPath)
import sco2_plots as cy_plt
import PySSC as sscapi
import ssc_inout_v2 as ssc_sim
import sco2_cycle_ssc as sco2_solve

htr_pp_left_label = "htr_pp_left"
htr_pp_right_label = "htr_pp_right"
ltr_pp_left_label = "ltr_pp_left"
ltr_pp_right_label = "ltr_pp_right"


def write_string_array(file_name, list_of_list_string, delimiter):
    # this will be a list of lists
    N_col = len(list_of_list_string)
    N_row = len(list_of_list_string[0])

    f = open(file_name, "w")

    for row in range(N_row):

        for col in range(N_col):
            val = list_of_list_string[col][row]
            f.write(val)
            
            if(col != N_col - 1):
                f.write(delimiter)
        
        f.write('\n')
    
    f.close()

def write_dict(file_name, result_dict, delimiter):

    f = open(file_name, "w")

    for key in result_dict:
        f.write(key)
        f.write(delimiter)

        if isinstance(result_dict[key], list):
            NVal = len(result_dict[key])
            for i in range(NVal):
                val = result_dict[key][i]
                f.write(str(val))
                
                if i != NVal - 1:
                    f.write(delimiter)
        else:
            val = result_dict[key]
            f.write(str(val))
            f.write(delimiter)

        f.write('\n')

    f.close()

def example_plot():
    fig, ax = plt.subplots(subplot_kw={"projection": "3d"})

    # Make data.
    X = np.arange(-5, 5, 0.25)
    Y = np.arange(-5, 5, 0.25)
    X, Y = np.meshgrid(X, Y)
    R = np.sqrt(X**2 + Y**2)
    Z = np.sin(R)

    # Plot the surface.
    surf = ax.plot_surface(X, Y, Z, cmap=cm.coolwarm,
                        linewidth=0, antialiased=False)

    # Customize the z axis.
    ax.set_zlim(-1.01, 1.01)
    ax.zaxis.set_major_locator(LinearLocator(10))
    # A StrMethodFormatter is used automatically
    ax.zaxis.set_major_formatter('{x:.02f}')

    # Add a color bar which maps values to colors.
    fig.colorbar(surf, shrink=0.5, aspect=5)

    plt.show()

def sigmoid(val):
    return 1.0/(1.0 + np.exp(-val))

def calculate_obj(eta_list, temp_list, target_temp, inlet_temp):
    obj_list = []

    if(len(eta_list) != len(temp_list)):
        return
    
    N = len(temp_list)
    temp_span = inlet_temp - target_temp

    for i in range(N):
        temp_error = np.abs(temp_list[i] - target_temp)
        percent_error = temp_error / temp_span
        penalty = 10.0 * (sigmoid(percent_error) - 0.5)

        obj = eta_list[i] - penalty
        obj_list.append(obj)
    
    return obj_list
    
def remove_zeros(x_list, y_list, z_list):
    zipped = list(zip(x_list, y_list, z_list))
    zipped = list(filter(lambda a: a != (0.0,0.0,0.0), zipped))
    x = [pair[0] for pair in zipped]
    y = [pair[1] for pair in zipped]
    z = [pair[2] for pair in zipped]

    return x,y,z

def remove_zeros_v2(result_ind_lists, skipfirstrow = False):
    N_vals = len(result_ind_lists)
    no_zero_list = []
    if(skipfirstrow == True):
        no_zero_list.append(result_ind_lists[0])
    count = 0


    for result in result_ind_lists:
        if(skipfirstrow == True and count == 0):
            count += 1
            continue

        isZero = True
        i = 0
        for val in result:
            if val != '' and float(val) != 0.0 and i > 6:
                isZero = False
                break
            i+=1



        if isZero == False:     
            no_zero_list.append(result)

        count += 1


    
    return no_zero_list

def remove_zeros_from_dict(result_dict):
    N_vals = len(result_dict[list(result_dict.keys())[0]])
    
    non_zero_dict = {}
    for key in result_dict:
        non_zero_dict[key] = []

    for i in range(N_vals):
        
        # Check if failed run
        if(float(result_dict["h_state_points 0"][i]) != 0.0):
            for key in result_dict:
                non_zero_dict[key].append(result_dict[key][i])

    return non_zero_dict

def get_top_values(x_list, y_list, z_list):
    x_sorted, y_sorted, z_sorted = zip(*sorted(zip(x_list, y_list, z_list), reverse=False))

    topx_index_list = []
    currentX = -1
    current_top_Y = -1
    index_count = -1


    nVal = len(x_sorted)
    for i in range(nVal):
        x = x_sorted[i]
        y = y_sorted[i]
        z = z_sorted[i]

        if(x != currentX):
            currentX = x
            index_count += 1
            topx_index_list.append(i)
            current_top_Y = y
            continue

        if(y > current_top_Y):
            topx_index_list[index_count] = i
            current_top_Y = y

    

    top_pairs = []
    for i in topx_index_list:
        top_pairs.append((x_sorted[i], y_sorted[i], z_sorted[i]))


    x_result = [pair[0] for pair in top_pairs]
    y_result = [pair[1] for pair in top_pairs]
    z_result = [pair[2] for pair in top_pairs]

    return x_result, y_result, z_result
    
def toString(val, n=-1):
    if(n==-1):
        return str(val)
    
    rounded = round(val, n)
    return str(rounded)

    return f'{s:.2f}'


def get_data_pts_from_result_list(result_list, X_label, Y_label, Z_label, removeZero = False):
    label_list = result_list[0]

    # Find Columns of Interest
    X_col = -1
    Y_col = -1
    Z_col = -1

    count = 0
    for label in label_list:
        if(label == X_label):
            X_col = count
        if(label == Y_label):
            Y_col = count
        if(label == Z_label):
            Z_col = count
        count += 1

    # Compile Data of interest
    count = 0
    X_vals = []
    Y_vals = []
    Z_vals = []
    for data_row in result_list:
        if(count == 0):
            count += 1
            continue

        X_vals.append(float(data_row[X_col]))
        Y_vals.append(float(data_row[Y_col]))
        Z_vals.append(float(data_row[Z_col]))

    X = X_vals
    Y = Y_vals
    Z = Z_vals

    if(removeZero == True):
        X,Y,Z = remove_zeros(X,Y,Z)

    return X,Y,Z

def get_data_pts_from_result_list_v2(result_list, labels, removeZero=False, orderby=""):
    label_list = result_list[0]

    # Find Columns of Interest
    X_col = -1
    Y_col = -1
    Z_col = -1

    N_labels = len(labels)
    label_index_list = [0 for i in range(N_labels)]
    
    data_lists = [[] for i in range(N_labels)]

    count = 0
    for label in label_list:
        
        if label in labels:
            i = labels.index(label)
            label_index_list[i] = count

        count += 1

    # Compile Data of interest
    count = 0
    for data_row in result_list:
        if(count == 0):
            count += 1
            continue

        for i in range(N_labels):
            val = float(data_row[label_index_list[i]])
            data_lists[i].append(val)
            
    if(removeZero == True):
        data_lists = remove_zeros_v2(data_lists)

    result_dict = {}
    for i in range(N_labels):
        label = labels[i]
        result_dict[label] = data_lists[i]

    return result_dict

def open_dict_fast(file_name, removeZero = False):

    result_dict = {}

    f = open(file_name, "r")

    for line in f:

        items = line.split('\t')
        key = items[0]

        if key == '':
            continue

        N_items = len(items) - 1
        value_list = [0] * N_items

        for i in range(1, N_items + 1):
            val = float(items[i])
            value_list[i-1] = val

        result_dict[key] = value_list
        

    f.close()

    if removeZero:
        result_dict = remove_zeros_from_dict(result_dict)

    return result_dict

def experimental1(file_name):

    f = open(file_name, "r")

    lines_raw = []
    for line in f:
        lines_raw.append(line)

    f.close()

    result_dict = {}


    for line in lines_raw:
        items = line.split('\t')


        key = items[0]

        if key == '':
            continue

        N_items = len(items)
        value_list = [0] * N_items
        
        for i in range(1, N_items):
            val = float(items[i])
            value_list[i-1] = val





    result_list = []
    row_count = len(lines_list)
    col_count = len(lines_list[0])
    result_row = 0

    for col in range(col_count):

        result_list.append([])
        for row in range(row_count):
            result_list[col].append(lines_list[row][col].replace('\n',''))

    # Remove Zeros if necessary
    if removeZeros:
        result_list = remove_zeros_v2(result_list, True)

    # Convert Result List into dictionary
    N_cols = len(result_list[0])
    N_rows = len(result_list)
    result_dict = {}
    for col in range(N_cols):
        label = result_list[0][col]
        if(label == ""):
            continue

        vals = []
        
        for row in range(N_rows):
            if(row == 0):
                continue
            if result_list[row][col] == '':
                vals.append(0)
            else:
                vals.append(float(result_list[row][col]))

        result_dict[label] = vals



    return result_dict

def get_dict_from_file(file_name, removeZeros=False):
    f = open(file_name, "r")

    lines_raw = []
    for line in f:
        lines_raw.append(line)

    f.close()

    lines_list = []
    for line in lines_raw:
        lines_list.append(line.split('\t'))

    result_list = []
    row_count = len(lines_list)
    col_count = len(lines_list[0])
    result_row = 0

    for col in range(col_count):

        result_list.append([])
        for row in range(row_count):
            result_list[col].append(lines_list[row][col].replace('\n',''))

    # Remove Zeros if necessary
    if removeZeros:
        result_list = remove_zeros_v2(result_list, True)

    # Convert Result List into dictionary
    N_cols = len(result_list[0])
    N_rows = len(result_list)
    result_dict = {}
    for col in range(N_cols):
        label = result_list[0][col]
        if(label == ""):
            continue

        vals = []
        
        for row in range(N_rows):
            if(row == 0):
                continue
            if result_list[row][col] == '':
                vals.append(0)
            else:
                vals.append(float(result_list[row][col]))

        result_dict[label] = vals



    return result_dict

def get_dict_from_file_w_STRING(file_name, removeZeros=False):
    f = open(file_name, "r")

    lines_raw = []
    for line in f:
        lines_raw.append(line)

    f.close()

    lines_list = []
    for line in lines_raw:
        lines_list.append(line.split('\t'))

    result_list = []
    row_count = len(lines_list)
    col_count = len(lines_list[0])
    result_row = 0

    for col in range(col_count):

        result_list.append([])
        for row in range(row_count):
            result_list[col].append(lines_list[row][col].replace('\n',''))

    # Remove Zeros if necessary
    if removeZeros:
        result_list = remove_zeros_v2(result_list, True)

    # Convert Result List into dictionary
    N_cols = len(result_list[0])
    N_rows = len(result_list)
    result_dict = {}
    for col in range(N_cols):
        label = result_list[0][col]
        if(label == ""):
            continue

        vals = []
        
        for row in range(N_rows):
            if(row == 0):
                continue
            if result_list[row][col] == '':
                vals.append('')

            else:
                vals.append(convert_string(result_list[row][col]))

        result_dict[label] = vals



    return result_dict

def convert_string(val_string):
        is_dec = val_string.isdecimal()
        if(is_dec):
            return int(val_string)
        else:
            try:
                return float(val_string)
            except:
                return val_string

def sort_by_key(result_dict, key_label, reverse=False):

    #sweepX, sweepY, sweepZ = zip(*sorted(zip(sweepX, sweepY, sweepZ), reverse=True))

    list_of_lengths = []
    i = 0
    correct_val = 0
    for key in result_dict:

        length = len(result_dict[key])
        list_of_lengths.append(length)
        
        if (i == 0):
            correct_val = length
        elif (length != correct_val):
            bad = 0


        
        i += 1

    # Get list of vals to sort by
    sort_vals_list = result_dict[key_label]
    sort_keys_list = [key_label]

    # Make big list, with sort vals first
    big_list = [sort_vals_list]

    for key in result_dict:
        if(key != key_label):
            big_list.append(result_dict[key])
            sort_keys_list.append(key)

    # Sort the big list
    zipped_big_list = zip(*big_list)
    sorted_big_list = sorted(zipped_big_list, reverse=reverse)

    # Convert back to dict
    new_sorted_dict = {}
    i = 0
    for key in sort_keys_list:
        new_sorted_dict[key] = [item[i] for item in sorted_big_list]
        i += 1

    return new_sorted_dict

def split_by_key(result_dict, key_label):
    
    # First sort by key
    sorted_dict = sort_by_key(result_dict, key_label)

    # Loop through, making a new dict every time the value changes
    current_val = -111111111111111111111111111111111
    current_dict = {}
    N_val = len(sorted_dict[key_label])
    list_of_dicts = []

    for i in range(N_val):
        val = sorted_dict[key_label][i]
        
        if(val == current_val):
            for key in sorted_dict:
                current_dict[key].append(sorted_dict[key][i])
        else:
            if(current_dict != {}):
                list_of_dicts.append(current_dict)

            current_dict = {}
            for key in sorted_dict:
                current_dict[key] = [sorted_dict[key][i]]
        
        current_val = val

    return list_of_dicts

def add_config_name(result_dict, cycle_config_in = -1):
    # Check if config name key is already in dict
    desc_key = "config_name"
    if desc_key in result_dict:
        return result_dict
    
    for key in result_dict:
        NVal = len(result_dict[key])
        break

    i = 0
    config_name_list = []
    cycle_config_list = []
    cycle_config = -1

    for i in range(NVal):

        if(cycle_config_in != -1):
            cycle_config = cycle_config_in
        else:
            cycle_config = result_dict["cycle_config"][i]

        bypass_frac = result_dict["bypass_frac"][i]
        recomp_frac = result_dict["recomp_frac"][i]

        config_name = ""

        if(cycle_config == 1):
            if(recomp_frac <= 0.0001):
                config_name = "simple"
            else:
                config_name = "recompression"

        elif(cycle_config == 3):
            if bypass_frac != 0 and recomp_frac <= 0.01:
                config_name = "simple split flow bypass"
            elif recomp_frac <= 0.01 and bypass_frac == 0:
                config_name = "simple"
            elif bypass_frac == 0:
                config_name = "recompression"
            else:
                config_name = "htr bp"

        else:
            if recomp_frac == 0:
                config_name = "intercooling"
            else:
                config_name = "partial"
        
        config_name_list.append(config_name)
        cycle_config_list.append(cycle_config)


    result_dict[desc_key] = config_name_list
    result_dict['cycle_config'] = cycle_config_list

    return result_dict


def combine_dict_by_key(result_dict_list, key_name, key_value):

    # Create a new dict formed from the list of dicts that have the specific key value
    return_dict = {}
    for dictionary in result_dict_list:
        for key in dictionary:
            if key in return_dict:
                pass
            else:
                return_dict[key] = []


    for result_dict in result_dict_list:
        NVal = 0
        for key in result_dict:
            NVal = len(result_dict[key])
            break
        
        i = 0
        for i in range(NVal):
            val = result_dict[key_name][i]
            if(val == key_value):
                for key in result_dict:
                    return_dict[key].append(result_dict[key][i])

    is_restart = True
    while(is_restart == True):
        is_restart = False
        for key in return_dict:
            if(len(return_dict[key]) == 0):
                return_dict.pop(key)
                is_restart = True
                break
        
    
    

    return return_dict

def split_by_key_UDPATED(result_dict, key_label):

    # Loop through every run, find number of each type
    unique_key_id_list = [-1] * len(result_dict[key_label])
    unique_key_val_list = []
    unique_key_NVal_list = []
    NVal_total = len(result_dict[key_label])
    for i in range(NVal_total):
        val = result_dict[key_label][i]

        try:
            id = unique_key_val_list.index(val)
        except:
            unique_key_val_list.append(val)
            unique_key_NVal_list.append(0)
            id = len(unique_key_val_list) - 1

        unique_key_id_list[i] = id
        unique_key_NVal_list[id] += 1
        
    # Make new split dicts
    list_of_dicts = []
    for NVal in unique_key_NVal_list:
        sub_dict = {}
        for key in result_dict:
            sub_dict[key] = [-1] * NVal
        list_of_dicts.append(sub_dict)

    # Fill new split dicts
    unique_key_last_id_list = [0] * len(unique_key_val_list)
    for i in range(NVal_total):
        id = unique_key_id_list[i]
        for key in result_dict:
            list_of_dicts[id][key][unique_key_last_id_list[id]] = result_dict[key][i]
        unique_key_last_id_list[id] += 1

    return list_of_dicts

def combine_dicts(result_dict_list):
    combined_dict = result_dict_list[0]
    NDicts = len(result_dict_list)
    for i in range(NDicts - 1):
        dict_id = i + 1
        for key in result_dict_list[dict_id]:
            for val in result_dict_list[dict_id][key]:
                combined_dict[key].append(val)

    return combined_dict

def combine_common_runs(result_dict_list, compare_key_list):
    NDict = len(result_dict_list)
    NVal_list = []
    for result_dict in result_dict_list:
        NVal_list.append(len(result_dict[list(result_dict.keys())[0]]))

    ordered_list = NVal_list.copy()
    ordered_list.sort()

    ordered_dict_list = []
    for NVal in ordered_list:
        id = NVal_list.index(NVal)
        NVal_list[id] = 0
        ordered_dict_list.append(result_dict_list[id])

    compare_dict = ordered_dict_list[0]
    # Loop through dictionaries (after the first)
    for i in range(NDict - 1):
        id = i + 1

        
        current_dict = ordered_dict_list[id]
        combined_dict = {}
        for key in current_dict:
            combined_dict[key] = []
        NVal = ordered_list[id]
        NVal_compare = len(compare_dict[list(compare_dict.keys())[0]])
        # Loop through every run current dictionary
        for run_id in range(NVal):
            # Loop through compare dictionary
            for compare_run_id in range(NVal_compare):              
                # Check key parameters
                is_equal = True
                for key in compare_key_list:
                    if(compare_data(current_dict[key][run_id], compare_dict[key][compare_run_id]) == False):
                        is_equal = False
                        break
                # Check if dicts are identical
                if(is_equal):
                    is_identical = True
                    for key in current_dict:
                        if(compare_data(current_dict[key][run_id], compare_dict[key][compare_run_id]) == False):
                            is_identical == False
                            break
                    # Add dict to combined dict
                    for key in combined_dict:
                        combined_dict[key].append(compare_dict[key][compare_run_id])
                    if(is_identical == False):
                        # Add both dicts since they are not identical
                        for key in combined_dict:
                            combined_dict[key].append(current_dict[key][run_id])

        compare_dict = combined_dict
            
    return compare_dict
                    
            

def compare_data(val1, val2):
    if(isinstance(val1, str)):
        return (val1 == val2)
    else:
        if(isinstance(val1, int) and isinstance(val2, int)):
            return val1 == val2
        else:
            return float(val1) == float(val2)

# argument is a list of lists. First list is labels
def plot_from_result_list(result_list, X_label, Y_label, Z_label):
    
    X,Y,Z = get_data_pts_from_result_list(result_list, X_label, Y_label, Z_label)

    ################################################# Scatter Plot
    fig = plt.figure()
    ax = fig.add_subplot(projection='3d')

    ax.scatter(X, Y, Z, c=Z,cmap='coolwarm')

    ax.set_xlabel(X_label)
    ax.set_ylabel(Y_label)
    ax.set_zlabel(Z_label)

    ################################################# 2D scatter with Z color
    fig3 = plt.figure()
    ax3 = fig3.add_subplot()

    cp3 = ax3.scatter(X, Y, c=Z,cmap='coolwarm')

    ax3.set_xlabel(X_label)
    ax3.set_ylabel(Y_label)

    cb3 = fig3.colorbar(cp3)
    cb3.set_label(Z_label)


    plt.show(block = True)

    ################################################# Surface Plot

    if False:
        # Find size
        x_is_flat = True
        if(X_vals[1] != X_vals[0]):
            x_is_flat = False

        size_array = X_vals
        intial_value = X_vals[0]
        if(x_is_flat == False):
            size_array = Y_vals
            intial_value = Y_vals[0]

        count = 0
        for val in size_array:
            if(val == intial_value):
                count += 1
            else:
                break

        #y_shape = count
        #x_shape = int(len(X_vals) / y_shape)
        #if(x_is_flat == False):
        #    x_shape = count
        #    y_shape = int(len(X_vals) / x_shape)

        shape = int(np.sqrt(len(X_vals)))
        x_shape = shape
        y_shape = shape

        # Arrange data for plotting
        X_surface = np.reshape(X_vals, (x_shape, y_shape))
        Y_surface = np.reshape(Y_vals, (x_shape, y_shape))
        Z_surface = np.reshape(Z_vals, (x_shape, y_shape))

        # Plot the surface.
        fig1, ax1 = plt.subplots(subplot_kw={"projection": "3d"})
        surf = ax1.plot_surface(X_surface, Y_surface, Z_surface, cmap=cm.coolwarm,
                            linewidth=0, antialiased=False)

        # Add a color bar which maps values to colors.
        fig1.colorbar(surf, shrink=0.5, aspect=5)

        ax1.set_xlabel(X_label)
        ax1.set_ylabel(Y_label)
        ax1.set_zlabel(Z_label)

        ################################################# Contour Plot
        fig2 = plt.figure()
        ax2 = fig2.add_subplot()
        #fig2,ax2 = plt.subplots(1,1)
        cp = ax2.contourf(X_surface, Y_surface, Z_surface, 25, cmap=cm.viridis)
        fig2.colorbar(cp)
        ax2.set_xlabel(X_label)
        ax2.set_ylabel(Y_label)

    
    asf = 0

def get_result_list_from_file(file_name):

    f = open(file_name, "r")

    lines_raw = []
    for line in f:
        lines_raw.append(line)

    f.close()

    lines_list = []
    for line in lines_raw:
        lines_list.append(line.split('\t'))

    result_list = []
    row_count = len(lines_list)
    col_count = len(lines_list[0])
    result_row = 0

    for col in range(col_count):

        result_list.append([])
        for row in range(row_count):
            result_list[col].append(lines_list[row][col].replace('\n',''))

    return result_list

def plot_from_file(file_name, X_label, Y_label, Z_label):

    result_list = get_result_list_from_file(file_name)
    plot_from_result_list(result_list, X_label, Y_label, Z_label)

def get_state_points_from_results_list(result_list, statename_string, index = 1):
    label_list = result_list[0]

    # Find Columns of Interest
    row_0 = -1

    count = 0
    for label in label_list:
        if(statename_string in label):
            row_0 = count
            break
        count += 1

    # Compile Data of interest
    count = 0
    vals = []

    data = result_list[index]
    current_row = row_0

    while (True):

        current_label = label_list[current_row]

        if(statename_string in current_label):
            val_string = data[current_row]
            if(val_string != 'nan'):
                val = float(val_string)
                vals.append(val)
            current_row += 1
        else:
            break

    return vals

def get_pareto_front(X_list, Y_list, is_max_X, is_max_Y):

    newX = []
    newY = []
    newZ = []

    nVal = len(X_list)
    for i in range(nVal):
        x = X_list[i]
        y = Y_list[i]
        
        if(x > 0 and y > 0):
            newX.append(x)
            newY.append(y)

    sorted_list = sorted([[newX[i], newY[i]] for i in range(len(newX))], reverse=is_max_X)

    pareto_front = [sorted_list[0]]

    for pair in sorted_list[1:]:
        if pair == [0.0,0.0]:
            continue

        if is_max_Y:
            if pair[1] >= pareto_front[-1][1]:
                pareto_front.append(pair)
        else:
            if pair[1] <= pareto_front[-1][1]:
                pareto_front.append(pair)


    pf_X = [pair[0] for pair in pareto_front]
    pf_Y = [pair[1] for pair in pareto_front]

    return pf_X, pf_Y

def get_pareto_dict(result_dict, X_label, Y_label, is_max_X, is_max_Y):

    # Sort by X_label
    sorted_dict = sort_by_key(result_dict, X_label, is_max_X)

    pareto_front = [[sorted_dict[X_label][0], sorted_dict[Y_label][0]],]
    pareto_index_list = [0]

    pareto_dict = {}
    for key in sorted_dict:
        pareto_dict[key] = [sorted_dict[key][0]]
    NVal = len(sorted_dict[X_label])

    for i in range(NVal):

        pair = [sorted_dict[X_label][i], sorted_dict[Y_label][i]]
        if pair == [0.0,0.0]:
            continue

        if is_max_Y:
            if pair[1] >= pareto_front[-1][1]:
                pareto_front.append(pair)
                for key in sorted_dict:
                    pareto_dict[key].append(sorted_dict[key][i])
        else:
            if pair[1] <= pareto_front[-1][1]:
                pareto_front.append(pair)
                for key in sorted_dict:
                    pareto_dict[key].append(sorted_dict[key][i])


    return pareto_dict


def get_pareto_front_from_dict_OLD(result_dict, X_label, Y_label, is_max_X, is_max_Y):
    
    sorted_dict = sort_by_key(result_dict, Y_label, False)
    x_pareto, y_pareto = get_pareto_front(result_dict[X_label], result_dict[Y_label], is_max_X, is_max_Y)
    y_pareto, x_pareto = zip(*sorted(zip(y_pareto, x_pareto), reverse=False))

    pareto_dict = {}
    for key in result_dict:
        pareto_dict[key] = []

    pareto_index = 0
    Nval = len(sorted_dict[list(sorted_dict.keys())[0]])
    for i in range(Nval):
        if (pareto_index < len(x_pareto)) and (sorted_dict[X_label][i] == x_pareto[pareto_index]) and (sorted_dict[Y_label][i] == y_pareto[pareto_index]):
            for key in result_dict:
                pareto_dict[key].append(sorted_dict[key][i])
            pareto_index += 1

    return pareto_dict


def get_pareto_front_from_dict(result_dict, X_label, Y_label, is_max_X, is_max_Y):
    
    sorted_dict = sort_by_key(result_dict, Y_label, False)
    x_pareto, y_pareto = get_pareto_front(result_dict[X_label], result_dict[Y_label], is_max_X, is_max_Y)
    y_pareto, x_pareto = zip(*sorted(zip(y_pareto, x_pareto), reverse=False))

    # Check sorted dict, x_pareto, and y_pareto
    is_sorted_dict_actually_sorted = is_sorted(sorted_dict[Y_label], True)
    is_x_pareto_actually_sorted = is_sorted(x_pareto, True)
    is_y_pareto_actually_sorted = is_sorted(y_pareto, True)

    if(len(x_pareto) != len(y_pareto)):
        print("Pareto front mismatch")

    N_pareto = len(x_pareto)

    #pareto_dict = {}
    #for key in result_dict:
    #    pareto_dict[key] = []
    ## Fill pareto dict with the cases that are on pareto front
    #pareto_index = 0
    #N_cases = len(sorted_dict[list(sorted_dict.keys())[0]])
    #
    #    # Loop through every case, check if on pareto front
    #i = 0
    #for i in range(N_cases):
    #    if (sorted_dict[X_label][i] == x_pareto[pareto_index]) and (sorted_dict[Y_label][i] == y_pareto[pareto_index]):
    #        for key in sorted_dict:
    #            pareto_dict[key].append(sorted_dict[key][i])
    #        pareto_index += 1

    pareto_dict_2 = {}
    for key in result_dict:
        pareto_dict_2[key] = []
    for pareto_index in range(N_pareto):
        sorted_index = get_first_combo_index(x_pareto[pareto_index], y_pareto[pareto_index], sorted_dict[X_label], sorted_dict[Y_label])

        if(sorted_index > -1):
            for key in sorted_dict:
                pareto_dict_2[key].append(sorted_dict[key][sorted_index])

    return pareto_dict_2


def get_first_index(val, val_vec):
    test = [i for i, x in enumerate(val_vec) if x == val]
    return test

def get_first_combo_index(val_x, val_y, val_vec_x, val_vec_y):
    x_index_list = [i for i, x in enumerate(val_vec_x) if x == val_x]
    y_index_list = [i for i, y in enumerate(val_vec_y) if y == val_y]

    for x_index in x_index_list:
        for y_index in y_index_list:
            if(x_index == y_index):
                return x_index
            
    return -1


def is_sorted(val_vec, ascend = True):

    # Check if values are sorted in ascending order
    prev_val = val_vec[0]
    if(ascend == True):
        for val in val_vec:
            if(val < prev_val):
                return False
            prev_val = val
            
        return True

    # Check if values are sorted in descending order
    else:
        for val in val_vec:
            if(val > prev_val):
                return False
            prev_val = val
        return True


def get_pareto_front_v2(X_list, Y_list, is_max_X, is_max_Y, Z_list = []):

    isZ = False
    if(Z_list != []):
        isZ = True


    nVal = len(X_list)
    
    # Combine X,Y,Z(opt) and remove (0,0)
    grouped_list = []
    if(isZ):
        grouped_list_total = list(zip(X_list, Y_list, Z_list))
        for i in range(nVal):
            if(grouped_list_total[i][0] != 0.0 or grouped_list_total[i][1] != 0.0):
                grouped_list.append(grouped_list_total[i])
    else:
        grouped_list = list(zip(X_list, Y_list))
        grouped_list.remove((0.0,0.0))

    # sort in order of X value
    sorted(grouped_list, reverse=is_max_X)
    
    #if(isZ):
        #sorted_list = sorted([[newX[i], newY[i]], newZ[i] for i in range(len(newX))], reverse=is_max_X)
    #else:
        #sorted_list = sorted([[newX[i], newY[i]] for i in range(len(newX))], reverse=is_max_X)

    pareto_front = [grouped_list[0]]

    for pair in grouped_list[1:]:
        if is_max_Y:
            if pair[1] >= pareto_front[-1][1]:
                pareto_front.append(pair)
        else:
            if pair[1] <= pareto_front[-1][1]:
                pareto_front.append(pair)


    pf_X = [pair[0] for pair in pareto_front]
    pf_Y = [pair[1] for pair in pareto_front]

    pf_Z = []
    if(isZ):
        pf_Z = [pair[2] for pair in pareto_front]




    return pf_X, pf_Y, pf_Z

def add_plot_data_to_result_dict(result_dict):
    cmod_name = "sco2_helper"

    id = thrd.get_native_id()

    dat = ssc_sim.dict_to_ssc_table(result_dict, cmod_name)

    val = ssc_sim.ssc_cmod(dat, cmod_name)

    flag = val[0]
    ssc_return_dict = val[1]

    if flag == True:
        for key in ssc_return_dict:
            if((key in result_dict) == False):
                result_dict[key] = ssc_return_dict[key]

    sscapi.PySSC().data_free(dat)

    return result_dict

def plot_Ts_via_result_dict(result_dict, dict_index):
    # Will Plot Ts from INDEX result in array (if more than one)
    state_point_labels = ["T_state_points", "P_state_points", "h_state_points", "s_state_points"]
    legacy_dict = {}
    for label in state_point_labels:
        legacy_dict[label] = [0] * 14

    for key in result_dict:

        is_state_point = False
        # Check if it is a state label
        for state_label in state_point_labels:
            if state_label in key:
                is_state_point = True
                # Split key string to get index
                key_splits = key.split()
                inner_index = int(key_splits[1])
                legacy_key = key_splits[0]

                # Place state point value
                legacy_dict[legacy_key][inner_index] = result_dict[key][dict_index]
                continue
        
        if is_state_point == False:
            legacy_dict[key] = result_dict[key][dict_index]

    if(("cycle_config" in legacy_dict) == False):
        legacy_dict['cycle_config'] = 3
        if(legacy_dict['config_name'] == 'partial'):
            legacy_dict['cycle_config'] = 2

    # Add 'Legacy' results needed for plotting
    legacy_dict["T_turb_in"] = legacy_dict["T_state_points"][5]
    legacy_dict["is_bypass_ok"] = -1 * float(legacy_dict["bypass_frac"])
    legacy_dict["is_recomp_ok"] = -1 * float(legacy_dict["recomp_frac"])

    legacy_dict = add_plot_data_to_result_dict(legacy_dict)

    c_plot = cy_plt.C_sco2_cycle_TS_plot(legacy_dict)
    c_plot.is_annotate = False
    c_plot.plot_new_figure()


def update_annot(ind, result_dict, label_list, annot, sc):
    pos = sc.get_offsets()[ind["ind"][0]]
    annot.xy = pos

    index = ind["ind"][0]

    N_label = len(label_list)
    text = ""
    for i in range(N_label):
        label = label_list[i]
        text += label + "= "
        if label in result_dict:
            res = isinstance(result_dict[label][index], str)
            if res == True:
                text += result_dict[label][index]
            else:
                text += toString(result_dict[label][index], 4)

        # Special Labels
        pp = -777
        if label == htr_pp_left_label:
            # MIXER_OUT - HTR_LP_OUT
            pp = result_dict["T_state_points 3"][index] - result_dict["T_state_points 7"][index]
            
        elif label == htr_pp_right_label:
            # HTR_HP_OUT - TURB_OUT
            pp = result_dict["T_state_points 4"][index] - result_dict["T_state_points 6"][index]

        elif label == ltr_pp_left_label:
            # MC_OUT - LTR_LP_OUT
            pp = result_dict["T_state_points 1"][index] - result_dict["T_state_points 8"][index]

        elif label == ltr_pp_right_label:
            # LTR_HP_OUT - HTR_LP_OUT
            pp = result_dict["T_state_points 2"][index] - result_dict["T_state_points 7"][index]

        if pp != -777:
            text += toString(pp, 4)

        if(i != N_label - 1):
            text += '\n'

    annot.set_text(text)


    annot.get_bbox_patch().set_facecolor("white")
    annot.get_bbox_patch().set_alpha(1)         



def get_marker_list():
    marker_dict = Line2D.markers
    marker_list = []
    for key in marker_dict:
        marker_list.append(key)
    return marker_list


def print_result_dict(result_dict, index, fig):
    #fig = plt.figure()

    file, _ = qt_compat._getSaveFileName(fig.canvas.parent(),
    caption = "Save data point to txt", filter ='*.txt')

    if(file != ''):
        save_dict = {}
        for key in result_dict:
            if((key in result_dict) and (len(result_dict[key]) > index)):
                save_dict[key] = result_dict[key][index]

        write_dict(file, save_dict, '\t')    



def hover(event, result_dict_arg, label_list, fig, annot, ax, sc):
    vis = annot.get_visible()
    if event.inaxes == ax:
        cont, ind = sc.contains(event)
        if cont:
            update_annot(ind, result_dict_arg, label_list, annot, sc)
            annot.set_visible(True)
            fig.canvas.draw_idle()
        else:
            if vis:
                annot.set_visible(False)
                fig.canvas.draw_idle()

def hover_multiple_pts(event, result_dict_list, label_list, fig, annot, ax, sc_collection):
    vis = annot.get_visible()

    is_right = False
    if(event.button == MouseButton.RIGHT):
        is_right = True

    if event.inaxes == ax:

        is_contained = False
        sc_ID = 0
        for sc in sc_collection:

            cont, ind = sc.contains(event)
            if cont == True:
                is_contained = True
                break
            sc_ID += 1

        if is_contained:
            
            if(event.button == MouseButton.RIGHT):
                index = ind["ind"][0]
                print_result_dict(result_dict_list[sc_ID], index, fig)
            elif(event.button == MouseButton.MIDDLE):
                index = ind["ind"][0]
                plot_Ts_via_result_dict(result_dict_list[sc_ID], index)
            else:
                update_annot(ind, result_dict_list[sc_ID], label_list, annot, sc_collection[sc_ID])
                annot.set_visible(True)
                
            fig.canvas.draw_idle()
        else:
            if vis:
                annot.set_visible(False)
                fig.canvas.draw_idle()



def plot_scatter_pts(dict_list_with_kwarg, X_info, Y_info, Z_info = [], title="", figure_size=[], ax=0, show_legend=True, legend_loc="", show_Z_legend=True):

    marker_list = get_marker_list()

    # Process Labels
    X_label = ""
    Y_label = ""
    Z_label = ""

    X_unit = ""
    Y_unit = ""
    Z_unit = ""

    if(isinstance(X_info, list)):
        X_label = X_info[0]
        if len(X_info) > 0:
            X_unit = X_info[1]
    else:
        X_label = X_info

    if(isinstance(Y_info, list)):
        Y_label = Y_info[0]
        if len(Y_info) > 0:
            Y_unit = Y_info[1]
    else:
        Y_label = Y_info


    if Z_info != []:
        if(isinstance(Z_info, list)):
            Z_label = Z_info[0]
            if len(Z_info) > 0:
                Z_unit = Z_info[1]
        else:
            Z_label = Z_info

    # Make figure and axis if it doesn't exist already
    if(ax == 0):
        if(len(figure_size) == 2):
            fig = plt.figure(figsize=(figure_size[0],figure_size[1]))
        else:
            fig = plt.figure()
        
        if(ax == 0):
            ax = fig.add_subplot()        
    else:
        fig = ax.get_figure()
    i = 0
    dict_list = []
    for data in dict_list_with_kwarg:
        diction = data[0]

        kwarg_dict = {}
        if(len(data) > 1):
            kwarg_dict = data[1]
        if ('marker' in kwarg_dict) == False:
            kwarg_dict['marker'] = marker_list[i+1]
        if (('c' in kwarg_dict) == False) and (Z_label != ""):
            kwarg_dict['c'] = diction[Z_label]

        #sca = ax.scatter(diction[X_label], diction[Y_label], c=diction[Z_label], label=label, marker=marker_list[i+1])

        #if(len(kwarg_dict.keys() == 0)):
        #    sca.set_marker

        sca = ax.scatter(diction[X_label], diction[Y_label], cmap='coolwarm', **kwarg_dict)

        dict_list.append(diction)

        i += 1

    cp3 = ax.collections[0]

    if isinstance(X_info, list) and len(X_info) > 2:
        X_plot_label = X_info[2]
    else:
        X_plot_label = X_label

    if isinstance(Y_info, list) and len(Y_info) > 2:
        Y_plot_label = Y_info[2]
    else:
        Y_plot_label = Y_label

    if(X_unit != ""):
        X_plot_label += " (" + X_unit + ")"

    if(Y_unit != ""):
        Y_plot_label += " (" + Y_unit + ")"

    ax.set_xlabel(X_plot_label)
    ax.set_ylabel(Y_plot_label)

    if Z_label != "":
        
        if isinstance(Z_info, list) and len(Z_info) > 2:
            Z_plot_label = Z_info[2]
        else:
            Z_plot_label = Z_label

        if(Z_unit != ""):
            Z_plot_label += " (" + Z_unit + ")"

        if(show_Z_legend == True):
            cb3 = fig.colorbar(ax.collections[0])
            cb3.set_label(Z_plot_label)

    if(title != ""):
        ax.set_title(title)

    # Handle Legend business
    if(show_legend):
        if(legend_loc == ""):
            legend_loc = "upper left"
        legend = ax.legend(loc=legend_loc)

    # Handle Click Business
    annot = ax.annotate("",xy=(0,0), xytext=(-100,20), textcoords="offset points",
                             bbox=dict(boxstyle="round", fc="w"),
                             arrowprops=dict(arrowstyle="->"))
    annot.set_visible(False)
    
    label_list = ["cycle_config", "config_name", "T_htf_cold_des", "eta_thermal_calc", "recup_total_UA_calculated", "LTR_UA_calculated", 
                  "HTR_UA_calculated", "UA_BPX", "UA_PHX",
                  "cycle_cost", "mc_cost_bare_erected", "rc_cost_bare_erected", "pc_cost_bare_erected", "t_cost_bare_erected", "t2_cost_bare_erected", "LTR_cost_bare_erected", "HTR_cost_bare_erected",
                  "PHX_cost_bare_erected", "BPX_cost_bare_erected", "mc_cooler_cost_bare_erected", "pc_cooler_cost_bare_erected", "piping_inventory_etc_cost"]
    fig.canvas.mpl_connect("button_press_event", lambda event: hover_multiple_pts(event, dict_list, label_list, fig, annot, ax, ax.collections))

    return ax



def plot_lines(dict_list_with_kwarg, X_label, Y_label, Z_label = "", title=""):

    marker_list = get_marker_list()


    fig = plt.figure()
    ax = fig.add_subplot()        
    
    i = 0
    dict_list = []
    for data in dict_list_with_kwarg:
        diction = data[0]

        kwarg_dict = {}
        #if(len(data) > 1):
        #    kwarg_dict = data[1]
        #if ('marker' in kwarg_dict) == False:
        #    kwarg_dict['marker'] = marker_list[i+1]
        #if (('c' in kwarg_dict) == False) and (Z_label != ""):
        #    kwarg_dict['c'] = diction[Z_label]

        #sca = ax.scatter(diction[X_label], diction[Y_label], c=diction[Z_label], label=label, marker=marker_list[i+1])

        #if(len(kwarg_dict.keys() == 0)):
        #    sca.set_marker

        sca = ax.plot(diction[X_label], diction[Y_label], **kwarg_dict)

        dict_list.append(diction)

        i += 1

    #cp3 = ax.collections[0]

    ax.set_xlabel(X_label)
    ax.set_ylabel(Y_label)

    #if Z_label != "":
    #    cb3 = fig.colorbar(ax.collections[0])
    #    cb3.set_label(Z_label)

    plt.legend(loc='upper left')

    ax.set_xlabel(X_label)
    ax.set_ylabel(Y_label)

    if(title != ""):
        ax.set_title(title)

    #annot = ax.annotate("",xy=(0,0), xytext=(-100,20), textcoords="offset points",
    #                         bbox=dict(boxstyle="round", fc="w"),
    #                         arrowprops=dict(arrowstyle="->"))
    #annot.set_visible(False)

        

    #label_list = ["cycle_config", "T_htf_cold_des", "eta_thermal_calc", "recup_total_UA_assigned", "recup_total_UA_calculated", 
    #              "LTR_UA_calculated", "HTR_UA_calculated",
    #              "bypass_frac", "recomp_frac", "P_comp_in", "cycle_cost"]
    #fig.canvas.mpl_connect("button_press_event", lambda event: hover_multiple_pts(event, dict_list, label_list, fig, annot, ax, ax.collections))

def plot_split_lines(dict_list_with_kwarg, X_label, Y_label, Z_label, title=""):

    list_of_list_of_dicts = []
    i = 0
    for data in dict_list_with_kwarg:
        diction = data[0]

        split_dict_list = split_by_key(diction, Z_label)
        list_of_list_of_dicts.append([])
        for split_dict in split_dict_list:
             list_of_list_of_dicts[i].append(split_dict)

        #kwarg_dict = {}
        #if(len(data) > 1):
        #    kwarg_dict = data[1]
        #if (('c' in kwarg_dict) == False) and (Z_label != ""):
        #    kwarg_dict['c'] = diction[Z_label]

        i += 1

    fig = plt.figure()
    ax = fig.add_subplot()
    
    i = 0
    N_total = len(list_of_list_of_dicts[0])
    for dicti in list_of_list_of_dicts[0]:
        if (i < 20) or ((i / N_total) > 0.0 and i % 50 == 0):
            ax.plot(dicti[X_label], dicti[Y_label], label=('Total UA = ' + toString(dicti[Z_label][0],2)))
        i += 1

def combine_dicts(dict_list):
    # Combines dicts (only contains keys that all dicts have)

    total_dict = {}

    for key in dict_list[0]:
        is_common = True
        for dic in dict_list:
            if (key in dic) == False:
                is_common = False
                break
        if is_common == True:
            total_dict[key] = []

    for key in total_dict:
        for dic in dict_list:
            for val in dic[key]:
                total_dict[key].append(val)

    return total_dict

def reduce_result_dict(big_result_file_name, output_file_name, reduction_factor = 0.5):
    # Load in Big File
    result_dict = open_dict_fast(big_result_file_name)

    # Remove Zeros
    nonzero_result_dict = remove_zeros_from_dict(result_dict)

    # Sort by Temperature (low to high)
    sorted_result_dict = sort_by_key(nonzero_result_dict, "T_htf_cold_des")

    # Extract Pareto Front
    eta_label = "eta_thermal_calc"
    T_label = "T_htf_cold_des"
    eta_pareto_list, T_htf_pareto_list = get_pareto_front(sorted_result_dict[eta_label], sorted_result_dict[T_label], True, False)
    T_htf_pareto_list, eta_pareto_list = zip(*sorted(zip(T_htf_pareto_list, eta_pareto_list), reverse=False))


    # Initialize Reduced Dict
    reduced_dict = {}
    for key in sorted_result_dict:
        reduced_dict[key] = []

    # Loop Through Sorted Result Dict
    N_vals = len(sorted_result_dict[list(sorted_result_dict.keys())[0]])
    pareto_index = 0
    N_pareto = len(eta_pareto_list)
    modulo = round(1.0 / reduction_factor)
    for i in range(N_vals):

        isInclude = False

        # Check if this is a pareto value
        eta = sorted_result_dict[eta_label][i]
        T_htf = sorted_result_dict[T_label][i]

        if pareto_index < N_pareto and (eta == eta_pareto_list[pareto_index]) and (T_htf == T_htf_pareto_list[pareto_index]):
            pareto_index += 1
            isInclude = True
        elif (i % modulo == 0):
            isInclude = True
        elif (i < 100):
            isInclude = True

        if isInclude:
            for key in sorted_result_dict:
                reduced_dict[key].append(sorted_result_dict[key][i])

    write_dict(output_file_name, reduced_dict, '\t')



    



def display_Alfani_2020():

    # File names
    file_name_sweep = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_sweep20_results.txt"
    file_name_paper = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_paper_results.txt"
    file_name_recomp = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_recomp_results.txt"
    file_name_partial = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_opt_partial_results20230929_093306.txt"
    file_name_htropt = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_opt_htrbp_results20230929_093739.txt"

    file_name_htrbp_UATotal = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_UATotal_Target_sweep300_results20230926_202850.txt"
    file_name_htrbp_tempsweep = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_htrbp_temp_sweep500_results20230920_152543.txt"

    file_name_sweep_UATotal_recomp = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_recomp_UAtotal_sweep500_results20230919_164327.txt"
    file_name_sweep_UATotal_partial = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_partial_UAtotal_sweep500_results20230919_164808.txt"
    
    file_name_sweep_partial = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_partial_full_sweep100_REDUCED.txt"
    file_name_sweep_recomp = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_recomp_full_sweep100_REDUCED.txt"

    sweep_dict = get_dict_from_file(file_name_sweep, True)
    opt_paper_dict = get_dict_from_file(file_name_paper, True)
    opt_recomp_dict = get_dict_from_file(file_name_recomp, True)
    opt_partial_dict = get_dict_from_file(file_name_partial, True)
    opt_htrbp_dict = get_dict_from_file(file_name_htropt, True)
    
    sweep_recomp_dict = get_dict_from_file(file_name_sweep_recomp, True)
    sweep_partial_dict = get_dict_from_file(file_name_sweep_partial, True)
    UATotal_htrbp_dict = get_dict_from_file(file_name_htrbp_UATotal, True)
    UATotal_recomp_dict = get_dict_from_file(file_name_sweep_UATotal_recomp, True)
    UATotal_partial_dict = get_dict_from_file(file_name_sweep_UATotal_partial, True)

    tempsweep_htrbp_dict = get_dict_from_file(file_name_htrbp_tempsweep, True)
    

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_bp_out_des"
    Z_label = "bypass_frac"

    # Get Pareto Front
    pfsweep_X, pfsweep_Y = get_pareto_front(sweep_dict[X_label], sweep_dict[Y_label], True, False)

    pfsweeppartial_X, pfsweeppartial_Y = get_pareto_front(sweep_partial_dict[X_label], sweep_partial_dict["T_htf_cold_des"], True, False)
    pfsweeprecomp_X, pfsweeprecomp_Y = get_pareto_front(sweep_recomp_dict[X_label], sweep_recomp_dict["T_htf_cold_des"], True, False)

    ################################################# Scatter Plot

    if True:
        fig = plt.figure()
        ax = fig.add_subplot(projection='3d')


        #sweepX, sweepY, sweepZ = zip(*sorted(zip(sweepX, sweepY, sweepZ), reverse=True))
        sorted_sweep_dict = sort_by_key(sweep_dict, Z_label)

        ax.scatter(sorted_sweep_dict[X_label], sorted_sweep_dict[Y_label], sorted_sweep_dict[Z_label], c=sorted_sweep_dict[Z_label],cmap='coolwarm',label='sweep')
        ax.scatter(opt_paper_dict[X_label], opt_paper_dict[Y_label], opt_paper_dict[Z_label], c=opt_paper_dict[Z_label],cmap='coolwarm',  marker="v", label='paper')
        ax.scatter(opt_recomp_dict[X_label], opt_recomp_dict["T_htf_cold_des"], opt_recomp_dict[Z_label], c=opt_recomp_dict[Z_label],cmap='coolwarm',  marker="v", label='recomp')
        ax.scatter(opt_partial_dict[X_label], opt_partial_dict["T_htf_cold_des"], opt_partial_dict[Z_label], c=opt_partial_dict[Z_label],cmap='coolwarm',  marker="v", label='partial')
        ax.scatter(opt_htrbp_dict[X_label], opt_htrbp_dict[Y_label], opt_htrbp_dict[Z_label], c=opt_htrbp_dict[Z_label],cmap='coolwarm',  marker="v", label='htropt', s = 500)
        
        ax.set_xlabel(X_label)
        ax.set_ylabel(Y_label)
        ax.set_zlabel(Z_label)

    ################################################# 2D scatter with Z color
    
    if True:
        fig3 = plt.figure()
        ax3 = fig3.add_subplot()

        cp3 = ax3.scatter(sweep_dict[X_label], sweep_dict[Y_label], c=sweep_dict[Z_label],cmap='coolwarm', label='sweep')
        ax3.scatter(opt_paper_dict[X_label], opt_paper_dict[Y_label], c='cyan', marker="1", label='paper', s=300)
        ax3.scatter(opt_recomp_dict[X_label], opt_recomp_dict["T_htf_cold_des"], c='lime', marker="2", label='recomp', s=300)
        ax3.scatter(opt_partial_dict[X_label], opt_partial_dict["T_htf_cold_des"], c='fuchsia', marker="3", label='partial', s=300)
        ax3.scatter(opt_htrbp_dict[X_label], opt_htrbp_dict[Y_label], c='yellow', marker="4", label='htropt', s=300)
        #ax3.plot(pfsweep_X, pfsweep_Y, label='pareto')
        ax3.plot(pfsweeppartial_X, pfsweeppartial_Y, label='partial pareto')
        ax3.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')
        #ax3.scatter(bonusX, bonusY, c='yellow', marker="^", label='bonus')
        #ax3.scatter(tempsweep_htrbp_dict[X_label], tempsweep_htrbp_dict[Y_label], c=tempsweep_htrbp_dict[Z_label], cmap='coolwarm', marker="^", label='optimized sweep')

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    ################################################# 2D scatter with Z color REVERSED

    if True:
        fig3 = plt.figure()
        ax3 = fig3.add_subplot()


        ordered_sweep_dict = sort_by_key(sweep_dict, Z_label, True)
        cp3 = ax3.scatter(ordered_sweep_dict[X_label], ordered_sweep_dict[Y_label], c=ordered_sweep_dict[Z_label],cmap='coolwarm', label='sweep')
        ax3.scatter(opt_paper_dict[X_label], opt_paper_dict[Y_label], c='cyan', marker="1", label='paper', s=300)
        ax3.scatter(opt_recomp_dict[X_label], opt_recomp_dict["T_htf_cold_des"], c='lime', marker="2", label='recomp', s=300)
        ax3.scatter(opt_partial_dict[X_label], opt_partial_dict["T_htf_cold_des"], c='fuchsia', marker="3", label='partial', s=300)
        ax3.scatter(opt_htrbp_dict[X_label], opt_htrbp_dict[Y_label], c='yellow', marker="4", label='htropt', s=300)
        #ax3.plot(pfsweep_X, pfsweep_Y, label='pareto')
        #ax3.plot(pfsweeppartial_X, pfsweeppartial_Y, label='partial pareto')
        #ax3.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')
        #ax3.scatter(bonusX, bonusY, c='yellow', marker="^", label='bonus')

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    ################################################# 2D scatter with Z color Partial and Recomp
    
    if True:
        fig_sweeps = plt.figure()
        ax_sweeps = fig_sweeps.add_subplot()

        cp_sweeps = ax_sweeps.scatter(sweep_dict[X_label], sweep_dict[Y_label], c=sweep_dict[Z_label],cmap='coolwarm', label='sweep')
        ax_sweeps.scatter(sweep_recomp_dict[X_label], sweep_recomp_dict["T_htf_cold_des"], c='lime', marker="2", label='recomp', s=300)
        ax_sweeps.scatter(sweep_partial_dict[X_label], sweep_partial_dict["T_htf_cold_des"], c='fuchsia', marker="3", label='partial', s=300)
        

        ax_sweeps.set_xlabel(X_label)
        ax_sweeps.set_ylabel(Y_label)

        cb_sweeps = fig3.colorbar(cp_sweeps)
        cb_sweeps.set_label(Z_label)

        plt.legend(loc='upper left')

        annot_sweeps = ax_sweeps.annotate("",xy=(0,0), xytext=(-100,20), textcoords="offset points",
                             bbox=dict(boxstyle="round", fc="w"),
                             arrowprops=dict(arrowstyle="->"))
        annot_sweeps.set_visible(False)

        label_list = ["T_htf_cold_des", "eta_thermal_calc", "recup_total_UA_assigned", "recup_total_UA_calculated", "bypass_frac", "recomp_frac", "cycle_cost"]
        fig_sweeps.canvas.mpl_connect("button_press_event", lambda event: hover(event, sweep_dict, label_list, fig_sweeps, annot_sweeps, ax_sweeps, cp_sweeps))


    ################################################# Pareto Lines
    
    if False:
        fig4 = plt.figure()
        ax3 = fig4.add_subplot()

        #ax3.plot(pfsweep_X, pfsweep_Y, label='pareto')
        ax3.plot(pfsweeppartial_X, pfsweeppartial_Y, label='partial pareto')
        ax3.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')
        #ax3.plot(UATotal_partial_dict[X_label], UATotal_partial_dict[Y_label], label='partial vary UA total')
        #ax3.plot(ua_opt_recomp_dict[X_label], UAtotal_recomp_dict[Y_label], label='recomp vary UA total')

        y_sorted, x_sorted, z_sorted = zip(*sorted(zip(tempsweep_htrbp_dict[Y_label], tempsweep_htrbp_dict[X_label], tempsweep_htrbp_dict[Z_label]), reverse=False))
        ax3.plot(x_sorted, y_sorted, label='optimized sweep')

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        plt.legend(loc='upper left')

    ################################################# Objective Function Value

    if False:
        
        targets = [470,465,460,455,450,445,440,435,430,425,420]
        obj_lists = []


        fig4 = plt.figure()
        ax4 = fig4.add_subplot()    

        ax4.set_xlabel("bypass_frac")
        ax4.set_ylabel("obj function")

        for target in targets:
            obj_list = calculate_obj(sweep_dict["eta_thermal_calc"], sweep_dict["bypass_frac"], target, 640.0)
            xtop, ytop, ztop = get_top_values(sweep_dict["bypass_frac"], obj_list, sweep_dict["T_htf_bypass_out"])
            ax4.plot(xtop, ytop, label=target)
            
        plt.legend(loc='upper left')

        fig5 = plt.figure()
        ax5 = fig5.add_subplot()
        obj_list_1 = calculate_obj(sweep_dict["eta_thermal_calc"], sweep_dict["T_htf_bypass_out"], 461.20516647407237, 640.0)
        ax5.scatter(sweep_dict["eta_thermal_calc"], obj_list_1)
        ax5.set_ybound(0.4,0.5)
        #plt.legend(loc='upper left')
        ax5.set_xlabel("bypass_frac")
        ax5.set_ylabel("obj function")

        ax4.set_ybound(0.4,0.5)

    ################################################# 2D Scatter UA total
    
    if True:
        fig3 = plt.figure()
        ax3 = fig3.add_subplot()

        cp3 = ax3.scatter(sweep_dict[X_label], sweep_dict[Y_label], c=sweep_dict[Z_label],cmap='coolwarm', label='sweep')
        ax3.scatter(opt_paper_dict[X_label], opt_paper_dict[Y_label], c='cyan', marker="1", label='paper', s=300)
        #ax3.scatter(opt_recomp_dict[X_label], opt_recomp_dict[Y_label], c='lime', marker="2", label='recomp', s=300)
        #ax3.scatter(opt_partial_dict[X_label], opt_partial_dict[Y_label], c='fuchsia', marker="3", label='partial', s=300)
        #ax3.scatter(opt_htrbp_dict[X_label], opt_htrbp_dict[Y_label], c='yellow', marker="4", label='htropt', s=300)
        #ax3.plot(pfsweep_X, pfsweep_Y, label='pareto')
        #ax3.plot(pfsweeppartial_X, pfsweeppartial_Y, label='partial pareto')
        #ax3.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    ################################################# HTR Opt Ts Diagram

    if False:

        T_vals = get_state_points_from_results_list(htropt_results, "T_state_points")
        s_vals = get_state_points_from_results_list(htropt_results, "s_state_points")

        fig3 = plt.figure()
        ax3 = fig3.add_subplot()

        cp3 = ax3.scatter(s_vals, T_vals)

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    ################################################# UA Total Sweeps

    if True:

        fig3 = plt.figure()
        ax3 = fig3.add_subplot()

        tempsweep_htrbp_dict[Y_label], tempsweep_htrbp_dict[X_label] = zip(*sorted(zip(tempsweep_htrbp_dict[Y_label], tempsweep_htrbp_dict[X_label]), reverse=False))
        UATotal_recomp_dict[Y_label], UATotal_recomp_dict[X_label] = zip(*sorted(zip(UATotal_recomp_dict[Y_label], UATotal_recomp_dict[X_label]), reverse=False))
        UATotal_partial_dict[Y_label], UATotal_partial_dict[X_label] = zip(*sorted(zip(UATotal_partial_dict[Y_label], UATotal_partial_dict[X_label]), reverse=False))

        ax3.plot(tempsweep_htrbp_dict[X_label], tempsweep_htrbp_dict[Y_label], label='htr bp pareto')
        ax3.plot(UATotal_recomp_dict[X_label], UATotal_recomp_dict["T_htf_cold_des"], label='ua total recomp')
        ax3.plot(UATotal_partial_dict[X_label], UATotal_partial_dict["T_htf_cold_des"], label='ua total partial')
        ax3.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')
        #ax3.plot(pfsweep_X, pfsweep_Y, label='pareto')
        #ax3.plot(pfsweeppartial_X, pfsweeppartial_Y, label='partial pareto')
        #ax3.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')
        

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        #cb3 = fig3.colorbar(cp3)
        #cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    ################################################# Compare Optimized Sweep with Pareto (from full sweep)

    if False:
        fig3 = plt.figure()
        ax3 = fig3.add_subplot()
        sorted_tempsweep = sort_by_key(tempsweep_htrbp_dict, Z_label)

        cp3 = ax3.scatter(sorted_tempsweep[X_label], sorted_tempsweep[Y_label], c=sorted_tempsweep[Z_label], cmap='coolwarm', marker="^", label='optimized sweep')
        ax3.scatter(pfsweep_X, pfsweep_Y,label='pareto')
        
        
        

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    ################################################# UA Total Sweeps

    if False:

        fig3 = plt.figure()
        ax3 = fig3.add_subplot()
        
        cp3 = ax3.scatter(UATotal_htrbp_dict[X_label], UATotal_htrbp_dict[Y_label], c=UATotal_htrbp_dict[Z_label], cmap='coolwarm', marker="^", label='htr bp')
        #ax3.scatter(pfsweep_X, pfsweep_Y,label='pareto')
        
        
        

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label("Total UA Assigned")

        plt.legend(loc='upper left')

    ################################################# Hover Data Test

    if True:

        fig5 = plt.figure()
        ax5 = fig5.add_subplot()
        
        sc5 = ax5.scatter(UATotal_htrbp_dict["eta_thermal_calc"], UATotal_htrbp_dict["T_htf_cold_des"], c=UATotal_htrbp_dict["recup_total_UA_assigned"], cmap='coolwarm', marker="^", label='htr bp')
        ax5.set_xlabel(X_label)
        ax5.set_ylabel(Y_label)
        cb35 = fig5.colorbar(sc5)
        cb35.set_label("Total UA Assigned")

        plt.legend(loc='upper left')

        annot5 = ax5.annotate("",xy=(0,0), xytext=(-100,20), textcoords="offset points",
                             bbox=dict(boxstyle="round", fc="w"),
                             arrowprops=dict(arrowstyle="->"))
        annot5.set_visible(False)

        

        label_list = ["T_htf_cold_des", "eta_thermal_calc", "recup_total_UA_assigned", "recup_total_UA_calculated", "bypass_frac", "recomp_frac", "cycle_cost"]
        fig5.canvas.mpl_connect("motion_notify_event", lambda event: hover(event, UATotal_htrbp_dict, label_list, fig5, annot5, ax5, sc5))

    ################################################# Partial Cooling Results

    if False:

        # Display all partial cooling points, and optimal

        
        fig_partial = plt.figure()
        ax_partial = fig_partial.add_subplot()
        
        sc_partial = ax_partial.scatter(sweep_partial_dict["eta_thermal_calc"], sweep_partial_dict["T_htf_cold_des"], c=sweep_partial_dict["cycle_cost"], cmap='coolwarm', marker="^", label='htr bp')
        ax_partial.scatter(opt_partial_dict["eta_thermal_calc"], opt_partial_dict["T_htf_cold_des"])

        ax_partial.set_xlabel(X_label)
        ax_partial.set_ylabel(Y_label)
        cb_partial = fig_partial.colorbar(sc_partial)
        cb_partial.set_label("Total UA Assigned")

        plt.legend(loc='upper left')

        annot_partial = ax_partial.annotate("",xy=(0,0), xytext=(-100,20), textcoords="offset points",
                             bbox=dict(boxstyle="round", fc="w"),
                             arrowprops=dict(arrowstyle="->"))
        annot_partial.set_visible(False)

        

        label_list = ["T_htf_cold_des", "eta_thermal_calc", "recup_total_UA_assigned", "recup_total_UA_calculated", "bypass_frac", "recomp_frac", "cycle_cost"]
        fig_partial.canvas.mpl_connect("motion_notify_event", lambda event: hover(event, sweep_partial_dict, label_list, fig_partial, annot_partial, ax_partial, sc_partial))


    ################################################# UATotal Pareto

    if True:
        Xpf, Ypf = get_pareto_front(UATotal_htrbp_dict["eta_thermal_calc"], UATotal_htrbp_dict["T_htf_cold_des"], True, False)



        #labels = ["T_htf_cold_des","eta_thermal_calc","recomp_frac","recup_total_UA_assigned","recup_total_UA_calculated"]
        #result_dict = get_data_pts_from_result_list_v2(htrbp_UATotal_results, labels, True)

        fig3 = plt.figure()
        ax = fig3.add_subplot()
        
        sc = ax.plot(Xpf, Ypf, label='UA Pareto')
        ax.plot(pfsweep_X, pfsweep_Y, label='pareto')
        ax.plot(pfsweeppartial_X, pfsweeppartial_Y, label='partial pareto')
        ax.plot(pfsweeprecomp_X, pfsweeprecomp_Y, label='recomp pareto')

        ax.set_xlabel(X_label)
        ax.set_ylabel(Y_label)
        plt.legend(loc='upper left')
        
    ################################################# UATotal Comparison

    if True:

        list_of_dicts = split_by_key(UATotal_htrbp_dict, "recup_total_UA_assigned")
        Xpf, Ypf = get_pareto_front(UATotal_htrbp_dict["eta_thermal_calc"], UATotal_htrbp_dict["T_htf_cold_des"], True, False)

        fig2 = plt.figure()
        ax = fig2.add_subplot()
        
        i = 0
        N_total = len(list_of_dicts)
        for dicti in list_of_dicts:
            if (i < 20) or ((i / N_total) > 0.0 and i % 50 == 0):
                ax.plot(dicti["eta_thermal_calc"], dicti["T_htf_cold_des"], label=('Total UA = ' + toString(dicti["recup_total_UA_assigned"][0],2)))
            i += 1

        ax.set_xlabel(X_label)
        ax.set_ylabel(Y_label)
        plt.legend(loc='upper left')

    plt.show(block = True)

def display_Alfani_2020_Final():

    # Collect File Names
    htrbp_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_htrbp_results20231003_135846.txt"
    simple_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_simple_results20231003_161605.txt"
    recomp_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_recomp_results20231003_165153.txt"
    partial_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_partial_results20231004_090811.txt"
    paper_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_paper_results20231003_132559.txt"

    htrbp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_sweep20_results20231004_094346.txt"
    simple_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_simple_sweep100_results20231009_143550.txt"
    recomp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_sweep100_REDUCED.txt"
    partial_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_sweep100_REDUCED.txt"

    htrbp_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_UATotal_sweep100_results20231005_110032.txt"
    simple_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_simple_UATotal_sweep500_results20231007_165311.txt"
    recomp_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_UATotal_sweep500_results20231007_165331.txt"
    partial_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_UATotal_sweep500_results20231007_165643.txt"

    htrbp_tempsweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_tempsweep100_results20231011_115746.txt"
    recomp_tempsweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_tempsweep100_results20231017_163312.txt"
    partial_tempsweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_tempsweep100_results20231017_163814.txt"

    htrbp_complete_filename = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_htrbp_TotalUA_sweep10_results20231113_104801.txt"

    # Load Result Dicts
    htrbp_opt_dict = add_config_name(open_dict_fast(htrbp_opt_filename, True), 3)
    simple_opt_dict = add_config_name(open_dict_fast(simple_opt_filename, True), 1)
    recomp_opt_dict = add_config_name(open_dict_fast(recomp_opt_filename, True), 1)
    partial_opt_dict = add_config_name(open_dict_fast(partial_opt_filename, True), 2)
    paper_dict = open_dict_fast(paper_filename, True)

    htrbp_sweep_dict = add_config_name(open_dict_fast(htrbp_sweep_filename, True), 3)
    simple_sweep_dict = add_config_name(open_dict_fast(simple_sweep_filename, True), 1)
    recomp_sweep_dict = add_config_name(open_dict_fast(recomp_sweep_filename, True), 1)
    partial_sweep_dict = add_config_name(open_dict_fast(partial_sweep_filename, True), 2)

    htrbp_UATotal_dict = add_config_name(open_dict_fast(htrbp_UATotal_filename, True), 3)
    simple_UATotal_dict = add_config_name(open_dict_fast(simple_UATotal_filename, True), 1)
    recomp_UATotal_dict = add_config_name(open_dict_fast(recomp_UATotal_filename, True), 1)
    partial_UATotal_dict = add_config_name(open_dict_fast(partial_UATotal_filename, True), 2)

    htrbp_tempsweep_dict = add_config_name(open_dict_fast(htrbp_tempsweep_filename, True), 3)
    recomp_tempsweep_dict = add_config_name(open_dict_fast(recomp_tempsweep_filename, True), 1)
    partial_tempsweep_dict = add_config_name(open_dict_fast(partial_tempsweep_filename, True), 2)

    htrbp_complete_dict = add_config_name(open_dict_fast(htrbp_complete_filename, True), 3)


    #config_dict_list = split_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict], "config_name");

    htrbp_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "htr bp")

    simple_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "simple")
    
    recomp_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "recompression")

    partial_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "partial")

    intercooling_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "intercooling")

    simple_split_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "simple split flow bypass")



    htrbp_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "htr bp")
    simple_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "simple")
    recomp_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "recompression")
    partial_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "partial")
    intercooling_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "intercooling")
    simple_split_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "simple split flow bypass")
    

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    # Create Pareto Fronts
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)
    simple_pareto_dict = get_pareto_front_from_dict(simple_sweep_dict, X_label, Y_label, True, False)
    recomp_pareto_dict = get_pareto_front_from_dict(recomp_sweep_dict, X_label, Y_label, True, False)
    partial_pareto_dict = get_pareto_front_from_dict(partial_sweep_dict, X_label, Y_label, True, False)

    htrbp_tempsweep_pareto_dict = get_pareto_front_from_dict(htrbp_tempsweep_dict, X_label, Y_label, True, False)
    htrbp_complete_pareto_dict = get_pareto_front_from_dict(htrbp_complete_dict, X_label, Y_label, True, False)

    # Create UATotal Pareto Fronts
    htrbp_UATotal_pareto_dict = get_pareto_front_from_dict(htrbp_UATotal_dict, X_label, Y_label, True, False)
    simple_UATotal_pareto_dict = get_pareto_front_from_dict(simple_UATotal_dict, X_label, Y_label, True, False)
    recomp_UATotal_pareto_dict = get_pareto_front_from_dict(recomp_UATotal_dict, X_label, Y_label, True, False)
    partial_UATotal_pareto_dict = get_pareto_front_from_dict(partial_UATotal_dict, X_label, Y_label, True, False)

    # Create Best of All Paretos
    htrbp_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(htrbp_pareto_dict, htrbp_UATotal_pareto_dict), X_label, Y_label, True, False)
    simple_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(simple_pareto_dict, simple_UATotal_pareto_dict), X_label, Y_label, True, False)
    recomp_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(recomp_pareto_dict, recomp_UATotal_pareto_dict), X_label, Y_label, True, False)
    partial_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(partial_pareto_dict, partial_UATotal_pareto_dict), X_label, Y_label, True, False)

    # Create Split Pareto Fronts based on ACTUAL configs
    htrbp_UA_compiled_pareto_dict = get_pareto_front_from_dict(htrbp_UA_compiled_dict, X_label, Y_label, True, False)
    simple_UA_compiled_pareto_dict = get_pareto_front_from_dict(simple_UA_compiled_dict, X_label, Y_label, True, False)
    recomp_UA_compiled_pareto_dict = get_pareto_front_from_dict(recomp_UA_compiled_dict, X_label, Y_label, True, False)
    partial_UA_compiled_pareto_dict = get_pareto_front_from_dict(partial_UA_compiled_dict, X_label, Y_label, True, False)
    intercooling_UA_compiled_pareto_dict = get_pareto_front_from_dict(intercooling_UA_compiled_dict, X_label, Y_label, True, False)
    simple_split_UA_compiled_pareto_dict = get_pareto_front_from_dict(simple_split_UA_compiled_dict, X_label, Y_label, True, False)

    htrbp_compiled_pareto_dict = get_pareto_front_from_dict(htrbp_compiled_dict, X_label, Y_label, True, False)
    simple_compiled_pareto_dict = get_pareto_front_from_dict(simple_compiled_dict, X_label, Y_label, True, False)
    recomp_compiled_pareto_dict = get_pareto_front_from_dict(recomp_compiled_dict, X_label, Y_label, True, False)
    partial_compiled_pareto_dict = get_pareto_front_from_dict(partial_compiled_dict, X_label, Y_label, True, False)
    intercooling_compiled_pareto_dict = get_pareto_front_from_dict(intercooling_compiled_dict, X_label, Y_label, True, False)
    simple_split_compiled_pareto_dict = get_pareto_front_from_dict(simple_split_compiled_dict, X_label, Y_label, True, False)

    # Plotting Parameters
    small_pt_size = 5

    # DEBUG
    if True:
        min_pressure_htrbp = min(htrbp_pareto_dict['P_comp_in'])
        max_pressure_htrbp = max(htrbp_pareto_dict['P_comp_in'])

        min_recomp_htrbp = min(htrbp_pareto_dict['recomp_frac'])
        max_recomp_htrbp = max(htrbp_pareto_dict['recomp_frac'])

        min_UATotal_htrbp = min(htrbp_pareto_dict['recup_total_UA_calculated'])
        max_UATotal_htrbp = max(htrbp_pareto_dict['recup_total_UA_calculated'])

        min_pressure_simple = min(simple_pareto_dict['P_comp_in'])
        max_pressure_simple = max(simple_pareto_dict['P_comp_in'])

        min_UATotal_simple = min(simple_pareto_dict['recup_total_UA_calculated'])
        max_UATotal_simple = max(simple_pareto_dict['recup_total_UA_calculated'])

        min_pressure_recomp = min(recomp_pareto_dict['P_comp_in'])
        max_pressure_recomp = max(recomp_pareto_dict['P_comp_in'])

        min_recomp_recomp = min(recomp_pareto_dict['recomp_frac'])
        max_recomp_recomp = max(recomp_pareto_dict['recomp_frac'])

        min_UATotal_recomp = min(recomp_pareto_dict['recup_total_UA_calculated'])
        max_UATotal_recomp = max(recomp_pareto_dict['recup_total_UA_calculated'])

        min_pressure_partial = min(partial_pareto_dict['P_comp_in'])
        max_pressure_partial = max(partial_pareto_dict['P_comp_in'])

        min_pressure10_partial = min(partial_pareto_dict['P_state_points 10'])
        max_pressure10_partial = max(partial_pareto_dict['P_state_points 10'])

        min_recomp_partial = min(partial_pareto_dict['recomp_frac'])
        max_recomp_partial = max(partial_pareto_dict['recomp_frac'])

        min_UATotal_partial = min(partial_pareto_dict['recup_total_UA_calculated'])
        max_UATotal_partial = max(partial_pareto_dict['recup_total_UA_calculated'])

    # Plot Compiled Dictionary
    plot_scatter_pts([[htrbp_compiled_dict, {'label':"htrbp", 'c':'red', 'marker':'.'}],
                      [simple_compiled_dict, {'label':"simple", 'c':'blue', 'marker':'.'}],
                      [recomp_compiled_dict, {'label':"recomp", 'c':'green', 'marker':'.'}],
                      [partial_compiled_dict, {'label':"partial", 'c':'purple', 'marker':'.'}],
                      [intercooling_compiled_dict, {'label':"intercooling", 'c':'black', 'marker':'.'}],
                      [simple_split_compiled_dict, {'label':"simple split bypass", 'c':'orange', 'marker':'.'}]], 
                      X_info, Y_info, title="Compiled Sweep")

    # Compiled Pareto Fronts
    plot_scatter_pts([[htrbp_compiled_pareto_dict, {'label':"recomp htr bp", 'c':'red', 'marker':'.'}],
                      [simple_compiled_pareto_dict, {'label':"simple", 'c':'blue', 'marker':'.'}],
                      [recomp_compiled_pareto_dict, {'label':"recomp", 'c':'green', 'marker':'.'}],
                      [partial_compiled_pareto_dict, {'label':"partial", 'c':'purple', 'marker':'.'}],
                      [intercooling_compiled_pareto_dict, {'label':"simple intercooling", 'c':'black', 'marker':'.'}],
                      [simple_split_compiled_pareto_dict, {'label':"simple split bp", 'c':'orange', 'marker':'.'}]], 
                      X_info, Y_info, title="Fixed Conductance Sweep Pareto Fronts")

    # Compiled Pareto Fronts (Z Recomp Fraction)
    plot_scatter_pts([[htrbp_compiled_pareto_dict, {'label':"recomp htr bp", 'marker':'.'}],
                      [simple_compiled_pareto_dict, {'label':"simple", 'marker':'.'}],
                      [recomp_compiled_pareto_dict, {'label':"recomp", 'marker':'.'}],
                      [partial_compiled_pareto_dict, {'label':"partial", 'marker':'.'}],
                      [intercooling_compiled_pareto_dict, {'label':"simple intercooling", 'marker':'.'}],
                      [simple_split_compiled_pareto_dict, {'label':"simple split bp", 'marker':'.'}]], 
                      X_info, Y_info, ["recomp_frac", "", "Recompression Fraction"], 
                      title="Fixed Conductance Sweep Pareto Fronts")

    # Compiled Pareto Fronts (Z Bypass Fraction)
    plot_scatter_pts([[htrbp_compiled_pareto_dict, {'label':"recomp htr bp", 'marker':'.'}],
                      [simple_compiled_pareto_dict, {'label':"simple", 'marker':'.'}],
                      [recomp_compiled_pareto_dict, {'label':"recomp", 'marker':'.'}],
                      [partial_compiled_pareto_dict, {'label':"partial", 'marker':'.'}],
                      [intercooling_compiled_pareto_dict, {'label':"simple intercooling", 'marker':'.'}],
                      [simple_split_compiled_pareto_dict, {'label':"simple split bp", 'marker':'.'}]], 
                      X_info, Y_info, ["bypass_frac", "", "Bypass Fraction"], 
                      title="Fixed Conductance Sweep Pareto Fronts")

    # Compiled UA Sweep
    plot_scatter_pts([[htrbp_UA_compiled_dict, {'label':"htrbp", 'c':'red', 'marker':'.'}],
                      [simple_UA_compiled_dict, {'label':"simple", 'c':'blue', 'marker':'.'}],
                      [recomp_UA_compiled_dict, {'label':"recomp", 'c':'green', 'marker':'.'}],
                      [partial_UA_compiled_dict, {'label':"partial", 'c':'purple', 'marker':'.'}],
                      [intercooling_UA_compiled_dict, {'label':"intercooling", 'c':'lime', 'marker':'.'}],
                      [simple_split_UA_compiled_dict, {'label':"simple split bypass", 'c':'orange', 'marker':'.'}]], 
                      X_info, Y_info, title="Compiled UA Sweep")

    # Compiled UA Pareto
    plot_scatter_pts([[htrbp_UA_compiled_pareto_dict, {'label':"recomp htr bp", 'c':'red', 'marker':'.'}],
                      [simple_UA_compiled_pareto_dict, {'label':"simple", 'c':'blue', 'marker':'.'}],
                      [recomp_UA_compiled_pareto_dict, {'label':"recomp", 'c':'green', 'marker':'.'}],
                      [partial_UA_compiled_pareto_dict, {'label':"partial", 'c':'purple', 'marker':'.'}],
                      [intercooling_UA_compiled_pareto_dict, {'label':"simple intercooling", 'c':'black', 'marker':'.'}],
                      [simple_split_UA_compiled_pareto_dict, {'label':"simple split bp", 'c':'orange', 'marker':'.'}]], 
                      X_info, Y_info, title="Varied Conductance Sweep Pareto Fronts")

    # Compiled UA Pareto (with Recomp Z)
    plot_scatter_pts([[htrbp_UA_compiled_pareto_dict, {'label':"recomp htr bp", 'marker':'.'}],
                      [simple_UA_compiled_pareto_dict, {'label':"simple", 'marker':'.'}],
                      [recomp_UA_compiled_pareto_dict, {'label':"recomp", 'marker':'.'}],
                      [partial_UA_compiled_pareto_dict, {'label':"partial", 'marker':'.'}],
                      [intercooling_UA_compiled_pareto_dict, {'label':"simple intercooling", 'marker':'.'}],
                      [simple_split_UA_compiled_pareto_dict, {'label':"simple split bp", 'marker':'.'}]], 
                      X_info, Y_info, ["recomp_frac", "", "Recompression Fraction"],
                      title="Varied Conductance Sweep Pareto Fronts")

    # Compiled UA Pareto (with Bypass Z)
    plot_scatter_pts([[htrbp_UA_compiled_pareto_dict, {'label':"recomp htr bp", 'marker':'.'}],
                      [simple_split_UA_compiled_pareto_dict, {'label':"simple split bp", 'marker':'.'}]], 
                      X_info, Y_info, ["bypass_frac", "", "Bypass Fraction"],
                      title="Varied Conductance Sweep Pareto Fronts")

    # Compiled UA Pareto (with Bypass Z)
    plot_scatter_pts([[htrbp_UA_compiled_pareto_dict, {'label':"recomp htr bp", 'marker':'.'}],
                      [simple_UA_compiled_pareto_dict, {'label':"simple", 'marker':'.'}],
                      [recomp_UA_compiled_pareto_dict, {'label':"recomp", 'marker':'.'}],
                      [partial_UA_compiled_pareto_dict, {'label':"partial", 'marker':'.'}],
                      [intercooling_UA_compiled_pareto_dict, {'label':"simple intercooling", 'marker':'.'}],
                      [simple_split_UA_compiled_pareto_dict, {'label':"simple split bp", 'marker':'.'}]], 
                      X_info, Y_info, ["bypass_frac", "", "Bypass Fraction"],
                      title="Varied Conductance Sweep Pareto Fronts")

    # Plot HTR BP Sweep Split with Simple Split Flow
    plot_scatter_pts([[htrbp_compiled_dict, {'label':"recomp htr bp sweep", 'c':'lightskyblue', 'marker':'.', 's':small_pt_size}], 
                      [htrbp_compiled_pareto_dict, {'label':"recomp htr bp pareto", 'c':'blue', 'marker':"."}],
                      [simple_split_compiled_dict, {'label':"simple split bp sweep", 'c':'lightcoral', 'marker':".", 's':small_pt_size}],
                      [simple_split_compiled_pareto_dict, {'label':"simple split bp pareto", 'c':'red', 'marker':"."}]
                      
                      ], 
                      X_info, Y_info, title="HTR Bypass Sweep")

    # Plot Partial Sweep with Intercooling
    plot_scatter_pts([[partial_compiled_dict, {'label':"partial sweep", 'c':'lightskyblue', 'marker':'.', 's':small_pt_size}], 
                      [partial_compiled_pareto_dict, {'label':"partial pareto", 'c':'blue', 'marker':'.'}],
                      [intercooling_compiled_dict, {'label':"simple intercooling sweep", 'c':'lightcoral', 'marker':'.', 's':small_pt_size}],
                      [intercooling_compiled_pareto_dict, {'label':"simple intercooling pareto", 'c':'red', 'marker':'.'}]
                      ], 
                      X_info, Y_info, title="Partial Cooling Cycle Sweep")

    # Plot HTR BP Sweep
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [htrbp_pareto_dict, {'label':"htr bp pareto", 'c':'blue', 'marker':"1"}]], 
                      "T_state_points 3", "T_htf_cold_des", title="Recompression with HTR Bypass Sweep")

    # Plot Opt Points
    opt_dict_list = [[htrbp_opt_dict, {'label':"htr bp"}], [simple_opt_dict, {'label':"simple"}], 
                    [recomp_opt_dict, {'label':"recomp"}], [partial_opt_dict, {'label':"partial"}], 
                    [paper_dict, {'label':"paper"}]]
    
    plot_scatter_pts(opt_dict_list, X_label, Y_label, Z_label, "Optimal Cases")

    # Plot HTR Sweep with All Optimal Points
    # plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'marker':'.'}], 
    #                   [htrbp_opt_dict, {'label':"htr bp opt", 'c':'cyan', 'marker':"1", 's':300}],
    #                   [simple_opt_dict, {'label':"simple opt", 'c':'lime', 'marker':"2", 's':300}],
    #                   [recomp_opt_dict, {'label':"recomp opt", 'c':'fuchsia', 'marker':"3", 's':300}],
    #                   [partial_opt_dict, {'label':"partial opt", 'c':'black', 'marker':"4", 's':300}]
    #                   ], 
    #                   X_info, Y_info, Z_info, "HTR BP w/ Optimal Cases")

    # Plot HTR BP Sweep
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'c':'lightskyblue', 'marker':'.', 's':small_pt_size}], 
                      [htrbp_pareto_dict, {'label':"htr bp pareto", 'c':'blue', 'marker':"1"}]], 
                      X_info, Y_info, title="Recompression with HTR Bypass Sweep")

    # Plot Simple Sweep
    plot_scatter_pts([[simple_sweep_dict, {'label':"simple sweep", 'c':'lightskyblue', 'marker':'.', 's':small_pt_size}], 
                      [simple_pareto_dict, {'label':"simple pareto", 'c':'blue', 'marker':"1"}]], 
                      X_info, Y_info, title="Simple Cycle Sweep")

    # Plot Recomp Sweep
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'c':'lightskyblue', 'marker':'.', 's':small_pt_size}], 
                      [recomp_pareto_dict, {'label':"recomp pareto", 'c':'blue', 'marker':'1'}]], 
                      X_info, Y_info, title="Recompression Cycle Sweep")

    # Plot Partial Sweep
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep", 'c':'lightskyblue', 'marker':'.', 's':small_pt_size}], 
                      [partial_pareto_dict, {'label':"partial pareto", 'c':'blue', 'marker':'1'}]], 
                      X_info, Y_info, title="Partial Cooling Cycle Sweep")


    # Plot Pareto
    plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'c':'red', 'marker':'.'}],
                      [simple_pareto_dict, {'label':"simple pareto", 'c':'blue', 'marker':'.'}],
                      [recomp_pareto_dict, {'label':"recomp pareto", 'c':'green', 'marker':'.'}],
                      [partial_pareto_dict, {'label':"partial pareto", 'c':'purple', 'marker':'.'}]], 
                      X_info, Y_info, title="Pareto Fronts")

    # Plot UA Total Pareto fronts
    plot_scatter_pts([[htrbp_UATotal_pareto_dict, {'label':"htrbp", 'c':'red', 'marker':'.'}],
                      [simple_UATotal_pareto_dict, {'label':"simple",'c':'blue',  'marker':'1'}],
                      [recomp_UATotal_pareto_dict, {'label':"recomp", 'c':'green', 'marker':'2'}],
                      [partial_UATotal_pareto_dict, {'label':"partial", 'c':'purple', 'marker':'3'}]], 
                      X_info, Y_info, title="Varied Overall Conductance Pareto Fronts")

    # # Compare HTR BP Pareto with Optimal Sweep
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"sweep pareto", 'c':'red', 'marker':'.'}],
    #                   [htrbp_tempsweep_pareto_dict, {'label':"optimal pareto", 'c':'blue', 'marker':'.'}]], 
    #                   X_info, Y_info, title="Recompression with HTR Bypass Optimal Pareto Comparison")

    # # Compare HTR BP UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"htr bp manual pareto", 'marker':'.'}],
    #                   [htrbp_UATotal_pareto_dict, {'label':"htr bp UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "HTR BP Sweep Pareto vs UA Pareto")

    # # Compare Simple UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[simple_pareto_dict, {'label':"simple manual pareto", 'marker':'.'}],
    #                   [simple_UATotal_pareto_dict, {'label':"simple UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "Simple Sweep Pareto vs UA Pareto")

    # # Compare Recomp UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[recomp_pareto_dict, {'label':"recomp manual pareto", 'marker':'.'}],
    #                   [recomp_UATotal_pareto_dict, {'label':"recomp UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "Recomp Sweep Pareto vs UA Pareto")
    
    # Compare Partial UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[partial_pareto_dict, {'label':"partial manual pareto", 'marker':'.'}],
    #                   [partial_UATotal_pareto_dict, {'label':"partial UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "Partial Sweep Pareto vs UA Pareto")

    # HTR BP UATotal
    # plot_scatter_pts([[htrbp_UATotal_dict, {'label':"HTR BP UATotal", 'marker':'.'}]], 
    #                   X_label, [Y_label, Y_unit], ["recup_total_UA_calculated","MW/K"], "HTR BP UA Total")

    # HTR BP UATotal Split Lines
    # plot_split_lines([[htrbp_UATotal_dict, {'label':"HTR BP UATotal", 'marker':'.'}]], 
    #                   X_label, Y_label, "recup_total_UA_assigned", "HTR BP UA Total")


    # Temperature Sweep
    plot_scatter_pts([[htrbp_tempsweep_dict, {'label':"htr bp temperature sweep", 'c':'red', 'marker':'.'}],
                      [recomp_tempsweep_dict, {'label':"recomp temperature sweep", 'c':'blue', 'marker':'.'}],
                      [partial_tempsweep_dict, {'label':"partial temperature sweep", 'c':'green', 'marker':'.'}]
                      ], 
                      X_label, Y_label, title="Temperature Sweep")

    # Best of All Cycles
    # plot_scatter_pts([[htrbp_combined_pareto_dict, {'label':"HTR BP Best", 'marker':'.', 'c':'black'}],
    #                   [simple_combined_pareto_dict, {'label':"Simple Best", 'marker':'1', 'c':'lime'}],
    #                   [recomp_combined_pareto_dict, {'label':"Recomp Best", 'marker':'2', 'c':'cyan'}],
    #                   [partial_combined_pareto_dict, {'label':"Partial Best", 'marker':'3', 'c':'fuchsia'}]
    #                   ], 
    #                   X_label, Y_label,  title="HTR BP Best")

    # Costs
    # Plot Pareto
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'marker':'.'}],
    #                   [simple_pareto_dict, {'label':"simple pareto", 'marker':'.'}],
    #                   [recomp_pareto_dict, {'label':"recomp pareto", 'marker':'.'}],
    #                   [partial_pareto_dict, {'label':"partial pareto", 'marker':'.'}]], 
    #                   X_label, [Y_label, Y_unit], ["cycle_cost","M$"], "Pareto Fronts")

    # Compare HTR BP with HTR BP UA Total Sweep
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'marker':'1'}],
    #                   [htrbp_complete_dict, {'label':"htrbp complete pareto", 'marker':'.'}] ],
    #                   X_label, [Y_label, Y_unit], ["recup_total_UA_calculated","MW/K"], "Pareto Fronts")

    plt.show(block = True)

def display_Alfani_2020_Final_2():

    # Collect File Names
    htrbp_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_htrbp_results20231003_135846.txt"
    simple_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_simple_results20231003_161605.txt"
    recomp_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_recomp_results20231003_165153.txt"
    partial_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_partial_results20231004_090811.txt"
    paper_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_paper_results20231003_132559.txt"

    htrbp_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_htrbp_sweep20_results20231117_092217.txt"
    simple_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_simple_sweep20_results20231117_114303.txt"
    recomp_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_recomp_sweep20_results20231117_114311.txt"
    partial_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_partial_sweep20_results20231117_115355.txt"

    htrbp_COMPLETE_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_UATotal_sweep100_results20231005_110032.txt"
    simple_COMPLETE_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_simple_COMPLETE_sweep10_results20231117_151150.txt"
    recomp_COMPLETE_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_recomp_COMPLETE_sweep10_results20231117_151156.txt"
    partial_COMPLETE_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data 2\\alfani_2020_partial_COMPLETE_sweep10_results20231117_153459.txt"

    htrbp_tempsweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_tempsweep100_results20231011_115746.txt"
    recomp_tempsweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_tempsweep100_results20231017_163312.txt"
    partial_tempsweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_tempsweep100_results20231017_163814.txt"

    # Load Result Dicts
    htrbp_opt_dict = open_dict_fast(htrbp_opt_filename, True)
    simple_opt_dict = open_dict_fast(simple_opt_filename, True)
    recomp_opt_dict = open_dict_fast(recomp_opt_filename, True)
    partial_opt_dict = open_dict_fast(partial_opt_filename, True)
    paper_dict = open_dict_fast(paper_filename, True)

    htrbp_sweep_dict = open_dict_fast(htrbp_sweep_filename, True)
    simple_sweep_dict = open_dict_fast(simple_sweep_filename, True)
    recomp_sweep_dict = open_dict_fast(recomp_sweep_filename, True)
    partial_sweep_dict = open_dict_fast(partial_sweep_filename, True)

    htrbp_UATotal_dict = open_dict_fast(htrbp_COMPLETE_filename, True)
    simple_UATotal_dict = open_dict_fast(simple_COMPLETE_filename, True)
    recomp_UATotal_dict = open_dict_fast(recomp_COMPLETE_filename, True)
    partial_UATotal_dict = open_dict_fast(partial_COMPLETE_filename, True)

    htrbp_tempsweep_dict = open_dict_fast(htrbp_tempsweep_filename, True)
    recomp_tempsweep_dict = open_dict_fast(recomp_tempsweep_filename, True)
    partial_tempsweep_dict = open_dict_fast(partial_tempsweep_filename, True)

    min_pressure_partial = min(partial_sweep_dict['P_state_points 10'])
    max_pressure_partial = max(partial_sweep_dict['P_state_points 10'])

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    # Create Pareto Fronts
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)
    simple_pareto_dict = get_pareto_front_from_dict(simple_sweep_dict, X_label, Y_label, True, False)
    recomp_pareto_dict = get_pareto_front_from_dict(recomp_sweep_dict, X_label, Y_label, True, False)
    partial_pareto_dict = get_pareto_front_from_dict(partial_sweep_dict, X_label, Y_label, True, False)

    htrbp_tempsweep_pareto_dict = get_pareto_front_from_dict(htrbp_tempsweep_dict, X_label, Y_label, True, False)

    # Create UATotal Pareto Fronts
    htrbp_UATotal_pareto_dict = get_pareto_front_from_dict(htrbp_UATotal_dict, X_label, Y_label, True, False)
    simple_UATotal_pareto_dict = get_pareto_front_from_dict(simple_UATotal_dict, X_label, Y_label, True, False)
    recomp_UATotal_pareto_dict = get_pareto_front_from_dict(recomp_UATotal_dict, X_label, Y_label, True, False)
    partial_UATotal_pareto_dict = get_pareto_front_from_dict(partial_UATotal_dict, X_label, Y_label, True, False)

    # Create Best of All Paretos
    htrbp_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(htrbp_pareto_dict, htrbp_UATotal_pareto_dict), X_label, Y_label, True, False)
    simple_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(simple_pareto_dict, simple_UATotal_pareto_dict), X_label, Y_label, True, False)
    recomp_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(recomp_pareto_dict, recomp_UATotal_pareto_dict), X_label, Y_label, True, False)
    partial_combined_pareto_dict = get_pareto_front_from_dict(combine_dicts(partial_pareto_dict, partial_UATotal_pareto_dict), X_label, Y_label, True, False)

    # Plotting Parameters
    small_pt_size = 5

    # Plot Opt Points
    opt_dict_list = [[htrbp_opt_dict, {'label':"htr bp"}], [simple_opt_dict, {'label':"simple"}], 
                    [recomp_opt_dict, {'label':"recomp"}], [partial_opt_dict, {'label':"partial"}], 
                    [paper_dict, {'label':"paper"}]]
    
    plot_scatter_pts(opt_dict_list, X_label, Y_label, Z_label, "Optimal Cases")

    # Plot HTR Sweep with All Optimal Points
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'marker':'.'}], 
                      [htrbp_opt_dict, {'label':"htr bp opt", 'c':'cyan', 'marker':"1", 's':300}],
                      [simple_opt_dict, {'label':"simple opt", 'c':'lime', 'marker':"2", 's':300}],
                      [recomp_opt_dict, {'label':"recomp opt", 'c':'fuchsia', 'marker':"3", 's':300}],
                      [partial_opt_dict, {'label':"partial opt", 'c':'black', 'marker':"4", 's':300}]
                      ], 
                      [X_label, X_unit], [Y_label, Y_unit], [Z_label, Z_unit], "HTR BP w/ Optimal Cases")

    # Plot HTR BP Sweep
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [htrbp_pareto_dict, {'label':"htr bp pareto", 'c':'blue', 'marker':"1"}]], 
                      X_label, Y_label, title="Recompression with HTR Bypass Sweep")

    # Plot Simple Sweep
    plot_scatter_pts([[simple_sweep_dict, {'label':"simple sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [simple_pareto_dict, {'label':"simple pareto", 'c':'blue', 'marker':"1"}]], 
                      X_label, Y_label, title="Simple Cycle Sweep")

    # Plot Recomp Sweep
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [recomp_pareto_dict, {'label':"recomp pareto", 'c':'blue', 'marker':'1'}]], 
                      X_label, Y_label, title="Recompression Cycle Sweep")

    # Plot Partial Sweep
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [partial_pareto_dict, {'label':"partial pareto", 'c':'blue', 'marker':'1'}]], 
                      X_label, Y_label, title="Partial Cooling Cycle Sweep")


    # Plot Pareto
    plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'c':'red', 'marker':'.'}],
                      [simple_pareto_dict, {'label':"simple pareto", 'c':'blue', 'marker':'.'}],
                      [recomp_pareto_dict, {'label':"recomp pareto", 'c':'green', 'marker':'.'}],
                      [partial_pareto_dict, {'label':"partial pareto", 'c':'purple', 'marker':'.'}]], 
                      X_label, [Y_label, Y_unit], title="Pareto Fronts")

    # Plot UA Total Pareto fronts
    plot_scatter_pts([[htrbp_UATotal_pareto_dict, {'label':"htrbp", 'c':'red', 'marker':'.'}],
                      [simple_UATotal_pareto_dict, {'label':"simple",'c':'blue',  'marker':'1'}],
                      [recomp_UATotal_pareto_dict, {'label':"recomp", 'c':'green', 'marker':'2'}],
                      [partial_UATotal_pareto_dict, {'label':"partial", 'c':'purple', 'marker':'3'}]], 
                      X_label, [Y_label, Y_unit], title="Varied Overall Conductance Pareto Fronts")

    # Compare HTR BP Pareto with Optimal Sweep
    plot_scatter_pts([[htrbp_pareto_dict, {'label':"sweep pareto", 'c':'red', 'marker':'.'}],
                      [htrbp_tempsweep_pareto_dict, {'label':"optimal pareto", 'c':'blue', 'marker':'.'}]], 
                      X_label, [Y_label, Y_unit], title="Recompression with HTR Bypass Optimal Pareto Comparison")

    # Compare HTR BP UATotal Pareto with Sweep Pareto
    plot_scatter_pts([[htrbp_pareto_dict, {'label':"htr bp manual pareto", 'marker':'.'}],
                      [htrbp_UATotal_pareto_dict, {'label':"htr bp UATotal pareto", 'marker':'1', 's':300}]], 
                      X_label, Y_label, "recup_total_UA_calculated", "HTR BP Sweep Pareto vs UA Pareto")

    # Compare Simple UATotal Pareto with Sweep Pareto
    plot_scatter_pts([[simple_pareto_dict, {'label':"simple manual pareto", 'marker':'.'}],
                      [simple_UATotal_pareto_dict, {'label':"simple UATotal pareto", 'marker':'1', 's':300}]], 
                      X_label, Y_label, "recup_total_UA_calculated", "Simple Sweep Pareto vs UA Pareto")

    # Compare Recomp UATotal Pareto with Sweep Pareto
    plot_scatter_pts([[recomp_pareto_dict, {'label':"recomp manual pareto", 'marker':'.'}],
                      [recomp_UATotal_pareto_dict, {'label':"recomp UATotal pareto", 'marker':'1', 's':300}]], 
                      X_label, Y_label, "recup_total_UA_calculated", "Recomp Sweep Pareto vs UA Pareto")
    
    # Compare Partial UATotal Pareto with Sweep Pareto
    plot_scatter_pts([[partial_pareto_dict, {'label':"partial manual pareto", 'marker':'.'}],
                      [partial_UATotal_pareto_dict, {'label':"partial UATotal pareto", 'marker':'1', 's':300}]], 
                      X_label, Y_label, "recup_total_UA_calculated", "Partial Sweep Pareto vs UA Pareto")

    # HTR BP UATotal
    plot_scatter_pts([[htrbp_UATotal_dict, {'label':"HTR BP UATotal", 'marker':'.'}]], 
                      X_label, [Y_label, Y_unit], ["recup_total_UA_calculated","MW/K"], "HTR BP UA Total")

    # HTR BP UATotal Split Lines
    plot_split_lines([[htrbp_UATotal_dict, {'label':"HTR BP UATotal", 'marker':'.'}]], 
                      X_label, Y_label, "recup_total_UA_assigned", "HTR BP UA Total")


    # Temperature Sweep
    plot_scatter_pts([[htrbp_tempsweep_dict, {'label':"htr bp temperature sweep", 'c':'red', 'marker':'.'}],
                      [recomp_tempsweep_dict, {'label':"recomp temperature sweep", 'c':'blue', 'marker':'.'}],
                      [partial_tempsweep_dict, {'label':"partial temperature sweep", 'c':'green', 'marker':'.'}]
                      ], 
                      X_label, Y_label, title="Temperature Sweep")

    # Best of All Cycles
    plot_scatter_pts([[htrbp_combined_pareto_dict, {'label':"HTR BP Best", 'marker':'.', 'c':'black'}],
                      [simple_combined_pareto_dict, {'label':"Simple Best", 'marker':'1', 'c':'lime'}],
                      [recomp_combined_pareto_dict, {'label':"Recomp Best", 'marker':'2', 'c':'cyan'}],
                      [partial_combined_pareto_dict, {'label':"Partial Best", 'marker':'3', 'c':'fuchsia'}]
                      ], 
                      X_label, Y_label,  title="HTR BP Best")

    # Costs
    # Plot Pareto
    plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'marker':'.'}],
                      [simple_pareto_dict, {'label':"simple pareto", 'marker':'.'}],
                      [recomp_pareto_dict, {'label':"recomp pareto", 'marker':'.'}],
                      [partial_pareto_dict, {'label':"partial pareto", 'marker':'.'}]], 
                      X_label, [Y_label, Y_unit], ["cycle_cost","M$"], "Pareto Fronts")

    # Compare HTR BP with HTR BP UA Total Sweep
    plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'marker':'1'}],
                      [htrbp_UATotal_pareto_dict, {'label':"htrbp UATotal pareto", 'marker':'.'}] ],
                      X_label, [Y_label, Y_unit], ["recup_total_UA_calculated","MW/K"], "Pareto Fronts")

    plt.show(block = True)

def display_Alfani_2020_backup():
    
    htrbp_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 Validation\\alfani_2020_htrbp_sweep20_results20240122_140837.txt"
    simple_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 Validation\\alfani_2020_simple_sweep20_results20240122_163406.txt"
    recomp_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 Validation\\alfani_2020_recomp_sweep20_results20240122_163414.txt"
    partial_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 Validation\\alfani_2020_partial_sweep20_results20240122_164557.txt"

    htrbp_sweep_dict = open_dict_fast(htrbp_sweep_filename, True)
    simple_sweep_dict = open_dict_fast(simple_sweep_filename, True)
    recomp_sweep_dict = open_dict_fast(recomp_sweep_filename, True)
    partial_sweep_dict = open_dict_fast(partial_sweep_filename, True)

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    # Plotting Parameters
    small_pt_size = 5

    # Create Pareto Fronts
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)
    simple_pareto_dict = get_pareto_front_from_dict(simple_sweep_dict, X_label, Y_label, True, False)
    recomp_pareto_dict = get_pareto_front_from_dict(recomp_sweep_dict, X_label, Y_label, True, False)
    partial_pareto_dict = get_pareto_front_from_dict(partial_sweep_dict, X_label, Y_label, True, False)

    # Plot HTR BP Sweep
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [htrbp_pareto_dict, {'label':"htr bp pareto", 'c':'blue', 'marker':"1"}]], 
                      X_info, Y_info, title="Recompression with HTR Bypass Sweep")

    # Plot Simple Sweep
    plot_scatter_pts([[simple_sweep_dict, {'label':"simple sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [simple_pareto_dict, {'label':"simple pareto", 'c':'blue', 'marker':"1"}]], 
                      X_info, Y_info, title="Simple Cycle Sweep")

    # Plot Recomp Sweep
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [recomp_pareto_dict, {'label':"recomp pareto", 'c':'blue', 'marker':'1'}]], 
                      X_info, Y_info, title="Recompression Cycle Sweep")

    # Plot Partial Sweep
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [partial_pareto_dict, {'label':"partial pareto", 'c':'blue', 'marker':'1'}]], 
                      X_info, Y_info, title="Partial Cooling Cycle Sweep")

    #plt.show(block = True)

def display_Alfani_2020_BONUS():
    htrbp_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 BONUS\\alfani_2020_htrbp_UATotal_sweep50_results20240125_213634.txt"
    simple_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 BONUS\\alfani_2020_simple_UATotal_sweep50_results20240125_213513.txt"
    recomp_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 BONUS\\alfani_2020_recomp_UATotal_sweep50_results20240125_213014.txt"
    partial_sweep_filename = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020 BONUS\\alfani_2020_partial_UATotal_sweep50_results20240125_212639.txt"

    htrbp_sweep_dict = open_dict_fast(htrbp_sweep_filename, True)
    simple_sweep_dict = open_dict_fast(simple_sweep_filename, True)
    recomp_sweep_dict = open_dict_fast(recomp_sweep_filename, True)
    partial_sweep_dict = open_dict_fast(partial_sweep_filename, True)

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    # Plotting Parameters
    small_pt_size = 5

    # Create Pareto Fronts
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)
    simple_pareto_dict = get_pareto_front_from_dict(simple_sweep_dict, X_label, Y_label, True, False)
    recomp_pareto_dict = get_pareto_front_from_dict(recomp_sweep_dict, X_label, Y_label, True, False)
    partial_pareto_dict = get_pareto_front_from_dict(partial_sweep_dict, X_label, Y_label, True, False)

    # Plot HTR BP Sweep
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [htrbp_pareto_dict, {'label':"htr bp pareto", 'c':'blue', 'marker':"1"}]], 
                      X_info, Y_info, title="Recompression with HTR Bypass Sweep BONUS")

    # Plot Simple Sweep
    plot_scatter_pts([[simple_sweep_dict, {'label':"simple sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [simple_pareto_dict, {'label':"simple pareto", 'c':'blue', 'marker':"1"}]], 
                      X_info, Y_info, title="Simple Cycle Sweep BONUS")

    # Plot Recomp Sweep
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [recomp_pareto_dict, {'label':"recomp pareto", 'c':'blue', 'marker':'1'}]], 
                      X_info, Y_info, title="Recompression Cycle Sweep BONUS")

    # Plot Partial Sweep
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep", 'c':'gray', 'marker':'.', 's':small_pt_size}], 
                      [partial_pareto_dict, {'label':"partial pareto", 'c':'blue', 'marker':'1'}]], 
                      X_info, Y_info, title="Partial Cooling Cycle Sweep BONUS")

    plt.show(block = True)


def plot_dict_from_file(file_name, X_label, Y_label, Z_label=""):
    result_dict = open_dict_fast(file_name, True)
    plot_scatter_pts([[result_dict, {'marker':'.'}]], 
                      X_label, Y_label, Z_label)
    plt.show(block=True)

def compare_reduced(file_orig, file_reduced):

    # File names
    orig_dict = open_dict_fast(file_orig)
    orig_dict = remove_zeros_from_dict(orig_dict)
    reduced_dict = open_dict_fast(file_reduced)

    NVal = len(reduced_dict["bypass_frac"])
    for i in range(NVal):
        q_phx = reduced_dict["q_dot_PHX"]

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    ################################################# 2D scatter with Z color
    
    if True:
        fig3 = plt.figure()
        ax3 = fig3.add_subplot()

        cp3 = ax3.scatter(orig_dict[X_label], orig_dict[Y_label], c=orig_dict[Z_label],cmap='coolwarm', label='original')
        ax3.scatter(reduced_dict[X_label], reduced_dict[Y_label], c='cyan', marker="1", label='reduced', s=300)

        ax3.set_xlabel(X_label)
        ax3.set_ylabel(Y_label)

        cb3 = fig3.colorbar(cp3)
        cb3.set_label(Z_label)

        plt.legend(loc='upper left')

    plt.show(block = True)

def FindAnomalousDataPt():
    recomp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_sweep100_REDUCED.txt"
    recomp_dict = open_dict_fast(recomp_sweep_filename, True)

    NVal= len(recomp_dict["bypass_frac"])

    anomaly_dict = {}

    for i in range(NVal):
        eta = recomp_dict["eta_thermal_calc"][i]
        T = recomp_dict["T_htf_cold_des"][i]
        UATotal_calc = recomp_dict["recup_total_UA_calculated"][i]

        if (eta > 0.4301) and (eta < 0.4303):
            if (T > 408.1860) and (T < 408.1862):
                if (UATotal_calc > 30.996) and (UATotal_calc < 30.998):
                    this_is_it = 0

                    for key in recomp_dict:
                        anomaly_dict[key] = [recomp_dict[key][i]]

                    x = 56165

    file_out = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\anomaly.txt"
    write_dict(file_out, anomaly_dict, '\t')

def display_bp_opening():
    recomp_opt_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_opt_recomp_results20231003_165153.txt"
    bp_opening_filename = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_htrbp_opening_sweep100_results20231109_160751.txt"
    recomp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_sweep100_REDUCED.txt"
    recomp_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_UATotal_sweep500_results20231007_165331.txt"
    htrbp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_sweep20_results20231004_094346.txt"

    recomp_opt_dict = open_dict_fast(recomp_opt_filename, True)
    bp_opening_dict = open_dict_fast(bp_opening_filename, True)
    recomp_sweep_dict = open_dict_fast(recomp_sweep_filename, True)
    recomp_UATotal_dict = open_dict_fast(recomp_UATotal_filename, True)
    htrbp_sweep_dict = open_dict_fast(htrbp_sweep_filename, True)

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    recomp_pareto_dict = get_pareto_front_from_dict(recomp_sweep_dict, X_label, Y_label, True, False)
    recomp_UATotal_pareto_dict = get_pareto_front_from_dict(recomp_UATotal_dict, X_label, Y_label, True, False)
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)

    plot_scatter_pts([[bp_opening_dict, {'label':"bypass opening", 'marker':'.'}], 
                      [recomp_opt_dict, {'label':"recomp opt", 'c':'cyan', 'marker':"o"}],
                      [recomp_pareto_dict, {'label':"recomp pareto",'marker':"o"}],
                      [recomp_UATotal_pareto_dict, {'label':"recomp UA pareto", 'marker':"o"}],
                      [htrbp_pareto_dict, {'label':"htr bp pareto", 'marker':'.'}]
                      ], 
                      X_info, Y_info, Z_info, "HTR BP w/ Optimal Cases")
    
    plot_scatter_pts([[bp_opening_dict, {'label':"bypass opening", 'marker':'.'}], 
                      [recomp_pareto_dict, {'label':"recomp sweep \npareto",'c':'green', 'marker':"."}],
                      ], 
                      X_info, Y_info, Z_info, "HTR Bypass Effect")

    plt.show(block = True)

def display_UA_variation():
    htrbp_HIGHUA_filename = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_htrbp_sweep_highUATotal10_results20231111_101555.txt"
    htrbp_HIGHUA_dict = open_dict_fast(htrbp_HIGHUA_filename, True)

    htrbp_LOWUA_filename = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_htrbp_sweep_lowUATotal10_results20231111_104729.txt"
    htrbp_LOWUA_dict = open_dict_fast(htrbp_LOWUA_filename, True)

    htrbp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_sweep20_results20231004_094346.txt"
    htrbp_sweep_dict = open_dict_fast(htrbp_sweep_filename, True)

    htrbp_focused_filename = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_htrbp_sweep10_results20231111_112108.txt"
    htrbp_focused_dict = open_dict_fast(htrbp_focused_filename, True)

     # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    htrbp_HIGHUA_pareto_dict = get_pareto_front_from_dict(htrbp_HIGHUA_dict, X_label, Y_label, True, False)
    htrbp_LOWUA_pareto_dict = get_pareto_front_from_dict(htrbp_LOWUA_dict, X_label, Y_label, True, False)
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)
    htrbp_focused_pareto_dict = get_pareto_front_from_dict(htrbp_focused_dict, X_label, Y_label, True, False)

    plot_scatter_pts([[htrbp_HIGHUA_dict, {'label':"High UA", 'c':'blue', 'marker':'.'}], 
                      [htrbp_LOWUA_dict, {'label':"Low UA", 'c':'cyan', 'marker':"."}],
                      [htrbp_sweep_dict, {'label':"UA", 'c':'red', 'marker':"."}],
                      ], 
                      [X_label, ""], [Y_label, "C"], ["recup_total_UA_assigned", "MW/K"], "HTR BP w/ Optimal Cases")
    
    plot_scatter_pts([[htrbp_HIGHUA_pareto_dict, {'label':"High UA", 'c':'blue', 'marker':'.'}], 
                      [htrbp_LOWUA_pareto_dict, {'label':"Low UA", 'c':'cyan', 'marker':"."}],
                      [htrbp_pareto_dict, {'label':"UA", 'c':'red', 'marker':"."}],
                      ], 
                      [X_label, ""], [Y_label, "C"], ["recup_total_UA_assigned", "MW/K"], "HTR BP w/ Optimal Cases")
    
    plot_scatter_pts([ 
                      [htrbp_focused_dict, {'label':"Focused", 'c':'cyan', 'marker':"."}],
                      [htrbp_sweep_dict, {'label':"Regular", 'c':'red', 'marker':"."}],
                      ], 
                      [X_label, ""], [Y_label, "C"], ["recup_total_UA_assigned", "MW/K"], "HTR BP w/ Optimal Cases")

    plot_scatter_pts([ 
                      [htrbp_focused_pareto_dict, {'label':"Focused", 'c':'cyan', 'marker':"."}],
                      [htrbp_pareto_dict, {'label':"Regular", 'c':'red', 'marker':"."}],
                      ], 
                      [X_label, ""], [Y_label, "C"], ["recup_total_UA_assigned", "MW/K"], "HTR BP w/ Optimal Cases")

    plt.show(block = True)

def display_recomp():
    recomp_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_sweep100_REDUCED.txt"
    recomp_dict = open_dict_fast(recomp_filename, True)

    recomp_complete_filename = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_recomp_COMPLETE_sweep20_results20231122_100343.txt"
    recomp_complete_dict = open_dict_fast(recomp_complete_filename, True)

    recomp_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_UATotal_sweep500_results20231007_165331.txt"
    recomp_UATotal_dict = open_dict_fast(recomp_UATotal_filename, True)

     # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    recomp_pareto = get_pareto_front_from_dict(recomp_dict, X_label, Y_label, True, False)
    recomp_complete_pareto = get_pareto_front_from_dict(recomp_complete_dict, X_label, Y_label, True, False)
    recomp_UATotal_pareto = get_pareto_front_from_dict(recomp_UATotal_dict, X_label, Y_label, True, False)

    plot_scatter_pts([[recomp_pareto, {'label':"Normal", 'c':'blue', 'marker':'.'}], 
                      [recomp_complete_pareto, {'label':"Complete", 'c':'red', 'marker':"."}],
                      [recomp_UATotal_pareto, {'label':"Complete", 'c':'black', 'marker':"."}]], 
                      [X_label, ""], [Y_label, "C"], ["recup_total_UA_assigned", "MW/K"], "HTR BP w/ Optimal Cases")

    plt.show(block = True)

def display_Alfani_2024():
    fileaname = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2024_results10_20240206_224944.txt"
    

    data_dict = open_dict_fast(fileaname, True)
    
    #plot_Ts_via_result_dict(data_dict)

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    # Plotting Parameters
    small_pt_size = 5

    

    # Plot HTR BP Sweep
    plot_scatter_pts([[data_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                     "T_htf_cold_des", "T_state_points 3", "eta_thermal_calc", title="test", figure_size=[10,6])
    
    plt.show(block = True)

def special_investigation():

    # Collect File Names
    htrbp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_sweep20_results20231004_094346.txt"
    #htrbp_sweep_filename = r"C:\Users\tbrown2\Desktop\sco2_python\Alfani2020_Final\alfani_2020_htrbp_sweep4_results20240224_225518.txt"
    simple_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_simple_sweep100_results20231009_143550.txt"
    recomp_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_sweep100_REDUCED.txt"
    partial_sweep_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_sweep100_REDUCED.txt"

    htrbp_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_htrbp_UATotal_sweep100_results20231005_110032.txt"
    simple_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_simple_UATotal_sweep500_results20231007_165311.txt"
    recomp_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_recomp_UATotal_sweep500_results20231007_165331.txt"
    partial_UATotal_filename = "C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_UATotal_sweep500_results20231007_165643.txt"

    # Load Result Dicts
    htrbp_sweep_dict = add_config_name(open_dict_fast(htrbp_sweep_filename, True), 3)
    simple_sweep_dict = add_config_name(open_dict_fast(simple_sweep_filename, True), 1)
    recomp_sweep_dict = add_config_name(open_dict_fast(recomp_sweep_filename, True), 1)
    partial_sweep_dict = add_config_name(open_dict_fast(partial_sweep_filename, True), 2)

    htrbp_UATotal_dict = add_config_name(open_dict_fast(htrbp_UATotal_filename, True), 3)
    simple_UATotal_dict = add_config_name(open_dict_fast(simple_UATotal_filename, True), 1)
    recomp_UATotal_dict = add_config_name(open_dict_fast(recomp_UATotal_filename, True), 1)
    partial_UATotal_dict = add_config_name(open_dict_fast(partial_UATotal_filename, True), 2)


    #config_dict_list = split_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict], "config_name");

    htrbp_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "htr bp")

    simple_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "simple")
    
    recomp_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "recompression")

    partial_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "partial")

    intercooling_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "intercooling")

    simple_split_compiled_dict = combine_dict_by_key([htrbp_sweep_dict, simple_sweep_dict, recomp_sweep_dict, partial_sweep_dict],
                                              "config_name", "simple split flow bypass")



    htrbp_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "htr bp")
    simple_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "simple")
    recomp_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "recompression")
    partial_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "partial")
    intercooling_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "intercooling")
    simple_split_UA_compiled_dict = combine_dict_by_key([htrbp_UATotal_dict, simple_UATotal_dict, recomp_UATotal_dict, partial_UATotal_dict],
                                              "config_name", "simple split flow bypass")
    

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    # Create Pareto Fronts
    htrbp_pareto_dict = get_pareto_front_from_dict(htrbp_sweep_dict, X_label, Y_label, True, False)
    simple_pareto_dict = get_pareto_front_from_dict(simple_sweep_dict, X_label, Y_label, True, False)
    recomp_pareto_dict = get_pareto_front_from_dict(recomp_sweep_dict, X_label, Y_label, True, False)
    partial_pareto_dict = get_pareto_front_from_dict(partial_sweep_dict, X_label, Y_label, True, False)

    # Create UATotal Pareto Fronts
    htrbp_UATotal_pareto_dict = get_pareto_front_from_dict(htrbp_UATotal_dict, X_label, Y_label, True, False)
    simple_UATotal_pareto_dict = get_pareto_front_from_dict(simple_UATotal_dict, X_label, Y_label, True, False)
    recomp_UATotal_pareto_dict = get_pareto_front_from_dict(recomp_UATotal_dict, X_label, Y_label, True, False)
    partial_UATotal_pareto_dict = get_pareto_front_from_dict(partial_UATotal_dict, X_label, Y_label, True, False)

    # Create Split Pareto Fronts based on ACTUAL configs
    htrbp_UA_compiled_pareto_dict = get_pareto_front_from_dict(htrbp_UA_compiled_dict, X_label, Y_label, True, False)
    simple_UA_compiled_pareto_dict = get_pareto_front_from_dict(simple_UA_compiled_dict, X_label, Y_label, True, False)
    recomp_UA_compiled_pareto_dict = get_pareto_front_from_dict(recomp_UA_compiled_dict, X_label, Y_label, True, False)
    partial_UA_compiled_pareto_dict = get_pareto_front_from_dict(partial_UA_compiled_dict, X_label, Y_label, True, False)
    intercooling_UA_compiled_pareto_dict = get_pareto_front_from_dict(intercooling_UA_compiled_dict, X_label, Y_label, True, False)
    simple_split_UA_compiled_pareto_dict = get_pareto_front_from_dict(simple_split_UA_compiled_dict, X_label, Y_label, True, False)

    htrbp_compiled_pareto_dict = get_pareto_front_from_dict(htrbp_compiled_dict, X_label, Y_label, True, False)
    simple_compiled_pareto_dict = get_pareto_front_from_dict(simple_compiled_dict, X_label, Y_label, True, False)
    recomp_compiled_pareto_dict = get_pareto_front_from_dict(recomp_compiled_dict, X_label, Y_label, True, False)
    partial_compiled_pareto_dict = get_pareto_front_from_dict(partial_compiled_dict, X_label, Y_label, True, False)
    intercooling_compiled_pareto_dict = get_pareto_front_from_dict(intercooling_compiled_dict, X_label, Y_label, True, False)
    simple_split_compiled_pareto_dict = get_pareto_front_from_dict(simple_split_compiled_dict, X_label, Y_label, True, False)

    pres_in = "P_comp_in"
    htrbp_pareto_p_min = min(htrbp_pareto_dict[pres_in])
    recomp_pareto_p_min = min(recomp_pareto_dict[pres_in])
    htrbp_dict_p_min = min(htrbp_sweep_dict[pres_in])
    recomp_dict_p_min = min(recomp_sweep_dict[pres_in])

    # Plotting Parameters
    small_pt_size = 5

    z_pres = ["P_comp_in", "MPa", "Min Pressure"]
    z_bp = ["bypass_frac", "", "Bypass Fraction"]
    z_recomp = ["recomp_frac", "", "Recompression Fraction"]
    z_UA_frac = ["recup_LTR_UA_frac", "", "LTR UA Ratio"]

    # Plot Compiled Dictionary
    plot_scatter_pts([[htrbp_compiled_dict, {'label':"htrbp", 'c':'red', 'marker':'.'}],
                      [simple_compiled_dict, {'label':"simple", 'c':'blue', 'marker':'.'}],
                      [recomp_compiled_dict, {'label':"recomp", 'c':'green', 'marker':'.'}],
                      [partial_compiled_dict, {'label':"partial", 'c':'purple', 'marker':'.'}],
                      [intercooling_compiled_dict, {'label':"intercooling", 'c':'black', 'marker':'.'}],
                      [simple_split_compiled_dict, {'label':"simple split bypass", 'c':'orange', 'marker':'.'}]], 
                      X_info, Y_info, title="Compiled Sweep")

    plot_scatter_pts([[htrbp_compiled_pareto_dict, {'label':"recomp htr bp", 'c':'red', 'marker':'.'}],
                      [simple_compiled_pareto_dict, {'label':"simple", 'c':'blue', 'marker':'.'}],
                      [recomp_compiled_pareto_dict, {'label':"recomp", 'c':'green', 'marker':'.'}],
                      [partial_compiled_pareto_dict, {'label':"partial", 'c':'purple', 'marker':'.'}],
                      [intercooling_compiled_pareto_dict, {'label':"simple intercooling", 'c':'black', 'marker':'.'}],
                      [simple_split_compiled_pareto_dict, {'label':"simple split bp", 'c':'orange', 'marker':'.'}]], 
                      X_info, Y_info, title="Fixed Conductance Sweep Pareto Fronts")

    # Plot HTR BP Sweep
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_pres, title="Recompression with HTR Bypass Sweep")
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_bp, title="Recompression with HTR Bypass Sweep")
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_recomp, title="Recompression with HTR Bypass Sweep")
    plot_scatter_pts([[htrbp_sweep_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_UA_frac, title="Recompression with HTR Bypass Sweep")
    

    # Plot Simple Sweep
    plot_scatter_pts([[simple_sweep_dict, {'label':"simple sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_pres, title="Simple Cycle Sweep")

    # Plot Recomp Sweep
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_pres, title="Recompression Cycle Sweep")
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_recomp, title="Recompression Cycle Sweep")
    plot_scatter_pts([[recomp_sweep_dict, {'label':"recomp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_UA_frac, title="Recompression Cycle Sweep")

    # Plot Partial Sweep
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep",'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, ["P_state_points 10", "MPa", "Min Pressure"], title="Partial Cooling Cycle Sweep")
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep",'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_recomp, title="Partial Cooling Cycle Sweep")
    plot_scatter_pts([[partial_sweep_dict, {'label':"partial sweep",'marker':'.', 's':small_pt_size}], 
                      ], 
                      X_info, Y_info, z_UA_frac, title="Partial Cooling Cycle Sweep")


    # # Compare HTR BP Pareto with Optimal Sweep
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"sweep pareto", 'c':'red', 'marker':'.'}],
    #                   [htrbp_tempsweep_pareto_dict, {'label':"optimal pareto", 'c':'blue', 'marker':'.'}]], 
    #                   X_info, Y_info, title="Recompression with HTR Bypass Optimal Pareto Comparison")

    # # Compare HTR BP UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"htr bp manual pareto", 'marker':'.'}],
    #                   [htrbp_UATotal_pareto_dict, {'label':"htr bp UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "HTR BP Sweep Pareto vs UA Pareto")

    # # Compare Simple UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[simple_pareto_dict, {'label':"simple manual pareto", 'marker':'.'}],
    #                   [simple_UATotal_pareto_dict, {'label':"simple UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "Simple Sweep Pareto vs UA Pareto")

    # # Compare Recomp UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[recomp_pareto_dict, {'label':"recomp manual pareto", 'marker':'.'}],
    #                   [recomp_UATotal_pareto_dict, {'label':"recomp UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "Recomp Sweep Pareto vs UA Pareto")
    
    # Compare Partial UATotal Pareto with Sweep Pareto
    # plot_scatter_pts([[partial_pareto_dict, {'label':"partial manual pareto", 'marker':'.'}],
    #                   [partial_UATotal_pareto_dict, {'label':"partial UATotal pareto", 'marker':'1', 's':300}]], 
    #                   X_label, Y_label, "recup_total_UA_calculated", "Partial Sweep Pareto vs UA Pareto")

    # HTR BP UATotal
    # plot_scatter_pts([[htrbp_UATotal_dict, {'label':"HTR BP UATotal", 'marker':'.'}]], 
    #                   X_label, [Y_label, Y_unit], ["recup_total_UA_calculated","MW/K"], "HTR BP UA Total")

    # HTR BP UATotal Split Lines
    # plot_split_lines([[htrbp_UATotal_dict, {'label':"HTR BP UATotal", 'marker':'.'}]], 
    #                   X_label, Y_label, "recup_total_UA_assigned", "HTR BP UA Total")

    # Best of All Cycles
    # plot_scatter_pts([[htrbp_combined_pareto_dict, {'label':"HTR BP Best", 'marker':'.', 'c':'black'}],
    #                   [simple_combined_pareto_dict, {'label':"Simple Best", 'marker':'1', 'c':'lime'}],
    #                   [recomp_combined_pareto_dict, {'label':"Recomp Best", 'marker':'2', 'c':'cyan'}],
    #                   [partial_combined_pareto_dict, {'label':"Partial Best", 'marker':'3', 'c':'fuchsia'}]
    #                   ], 
    #                   X_label, Y_label,  title="HTR BP Best")

    # Costs
    # Plot Pareto
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'marker':'.'}],
    #                   [simple_pareto_dict, {'label':"simple pareto", 'marker':'.'}],
    #                   [recomp_pareto_dict, {'label':"recomp pareto", 'marker':'.'}],
    #                   [partial_pareto_dict, {'label':"partial pareto", 'marker':'.'}]], 
    #                   X_label, [Y_label, Y_unit], ["cycle_cost","M$"], "Pareto Fronts")

    # Compare HTR BP with HTR BP UA Total Sweep
    # plot_scatter_pts([[htrbp_pareto_dict, {'label':"htrbp pareto", 'marker':'1'}],
    #                   [htrbp_complete_dict, {'label':"htrbp complete pareto", 'marker':'.'}] ],
    #                   X_label, [Y_label, Y_unit], ["recup_total_UA_calculated","MW/K"], "Pareto Fronts")

    plt.show(block = True)


def example_plot_solve_dict():
    filename = r"C:\Users\tbrown2\Desktop\sco2_python\Alfani2020_Final\test_collection20240417_164418.csv"
    sim_collection = sco2_solve.C_sco2_sim_result_collection()
    sim_collection.open_csv(filename)

def demo_plotting():
    fileaname = "C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2024_results10_20240206_224944.txt"   

    data_dict = open_dict_fast(fileaname, True)

    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    X_real_name = "Thermal Efficiency"
    Y_real_name = "HTF Outlet Temperature"
    Z_real_name = "Bypass Fraction"

    X_info = [X_label, X_unit, X_real_name]
    Y_info = [Y_label, Y_unit, Y_real_name]
    Z_info = [Z_label, Z_unit, Z_real_name]

    # Plotting Parameters
    small_pt_size = 5    

    fig, (ax1, ax2) = plt.subplots(1,2)

    # Plot HTR BP Sweep
    plot_scatter_pts([[data_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                     "eta_thermal_calc", "T_htf_cold_des", figure_size=[10,6], ax=ax1, show_legend=False)
    
    plot_scatter_pts([[data_dict, {'label':"htr bp sweep", 'marker':'.', 's':small_pt_size}], 
                      ], 
                     "eta_thermal_calc", "T_htf_cold_des", figure_size=[10,6], ax=ax2, show_legend=True)


    plt.tight_layout()
    plt.show(block = True)

# Main Script

if __name__ == "__main__":
    #example_plot_solve_dict()

    #file_in = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_sweep100_results20231010_100456.txt"
    #file_out = "C:\\Users\\tbrown2\\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\Final Data\\alfani_2020_partial_sweep100_REDUCED.txt";
    #plot_dict_from_file(r"C:\Users\tbrown2\Desktop\sco2_python\Alfani2020_Final\alfani2020_2UA20240226_105819.txt", "eta_thermal_calc", "T_htf_cold_des")
    #reduce_result_dict(file_in, file_out, 0.2)
    #compare_reduced(file_in, file_out)
    #special_investigation()
    #display_Alfani_2024()
    #demo_plotting()
    #display_Alfani_2020()
    #display_recomp()
    #display_Alfani_2020_BONUS()
    #display_Alfani_2020_backup()
    display_Alfani_2020_Final()
    #display_Alfani_2020_Final_2()
    #FindAnomalousDataPt()
    #display_bp_opening()

    #plot_dict_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_simple_COMPLETE_sweep10_results20240112_103508.txt", "eta_thermal_calc", "T_htf_cold_des", "recup_total_UA_calculated")
    #plot_dict_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_partial_sweep20_results20231114_155525.txt", "eta_thermal_calc", "T_htf_cold_des", "recup_total_UA_calculated")

    #plot_dict_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_recomp_COMPLETE_sweep20_results20231122_100343.txt", "eta_thermal_calc", "T_htf_cold_des", "recup_total_UA_calculated")


    #display_UA_variation()
    #plot_dict_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\Alfani2020_Final\\alfani_2020_recomp_temp_target_sweep10_results20231109_143404.txt", "eta_thermal_calc", "T_htf_cold_des", "recup_total_UA_calculated")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua.txt", "recomp_frac", "LTR_UA_calculated", "eta_thermal_calc")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua1.txt", "eta_thermal_calc", "T_htf_bp_out_des", "bypass_frac")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua1.txt", "eta_thermal_calc", "T_htf_bp_out_des", "LTR_UA_calculated")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua1.txt", "eta_thermal_calc", "T_htf_bp_out_des", "recomp_frac")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua1.txt", "eta_thermal_calc", "T_htf_bp_out_des", "recomp_frac")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua_cost_16.txt", "eta_thermal_calc", "T_htf_bp_out_des", "cycle_cost")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua_pres_5.txt", "eta_thermal_calc", "T_htf_bp_out_des", "P_comp_in")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua_pres_20_b.txt", "eta_thermal_calc", "T_htf_bp_out_des", "P_comp_in")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\vary_bp_optimize_all_100.txt", "eta_thermal_calc", "T_htf_bp_out_des", "recomp_frac")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_recomp_ua_pres_20_b.txt", "eta_thermal_calc", "T_htf_bp_out_des", "P_comp_in")
    #plot_from_file("C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\bp_recomp_ua_pres_20_b.txt", "eta_thermal_calc", "T_htf_bp_out_des", "bypass_frac")
    #plot_from_file("C:\\Users\\tbrown2\\Desktop\\sco2_python\\bp_full_sweep_test_results20230912_120920.txt", "eta_thermal_calc", "T_htf_bp_out_des", "bypass_frac")
    #plot_from_file("C:\\Users\\tbrown2\OneDrive - NREL\\sCO2-CSP 10302.41.01.40\\Notes\\Optimization\\Alfani 2020\\alfani_2020_htrbp_temp_sweep10_results20230920_115557.txt", "eta_thermal_calc", "T_htf_bp_out_des", "bypass_frac")