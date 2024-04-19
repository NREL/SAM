import sys
import os
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.backend_bases import MouseButton

parentDir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(parentDir)

newPath = os.path.join(parentDir, 'core')
sys.path.append(newPath)
import sco2_cycle_ssc as sco2_solve

def toString(val, n=-1):
    if(n==-1):
        return str(val)
    
    rounded = round(val, n)
    return str(rounded)

    return f'{s:.2f}'

def get_marker_list():
    marker_dict = Line2D.markers
    marker_list = []
    for key in marker_dict:
        marker_list.append(key)
    return marker_list


def plot_scatter_pts(dict_list_with_kwarg, X_info, Y_info, Z_info = [], title=""):

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

    fig = plt.figure()
    ax = fig.add_subplot()        
    
    i = 0
    dict_list = []
    for data in dict_list_with_kwarg:
        #diction = data[0]

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

        X_val_list = []
        Y_val_list = []
        for d in data[0]:
            x = d[X_label]
            y = d[Y_label]
            if(isinstance(x, (int, float, complex)) and isinstance(y, (int, float, complex))):
                X_val_list.append(d[X_label])
                Y_val_list.append(d[Y_label])

        sca = ax.scatter(X_val_list, Y_val_list, cmap='coolwarm', **kwarg_dict)
        #dict_list.append(diction)

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

        cb3 = fig.colorbar(ax.collections[0])
        cb3.set_label(Z_plot_label)

    plt.legend(loc='upper left')

    if(title != ""):
        ax.set_title(title)
 
    annot = ax.annotate("",xy=(0,0), xytext=(-100,20), textcoords="offset points",
                             bbox=dict(boxstyle="round", fc="w"),
                             arrowprops=dict(arrowstyle="->"))
    annot.set_visible(False)






def example_plot_solve_dict():
    tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\Alfani2020_Final\TSF_G3P3_collection20240419_091842.csv"
    recomp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\Alfani2020_Final\recomp_G3P3_collection20240419_092151.csv"
    partial_filename = r"C:\Users\tbrown2\Desktop\sco2_python\Alfani2020_Final\partial_G3P3_collection20240419_092640.csv"


    tsf_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    tsf_sim_collection.open_csv(tsf_filename)

    recomp_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    recomp_sim_collection.open_csv(recomp_filename)

    partial_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    partial_sim_collection.open_csv(partial_filename)

    # Plot HTR Sweep with All Optimal Points
    plot_scatter_pts([[tsf_sim_collection.solve_dict_list, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.solve_dict_list, {'label':"recomp sweep", 'marker':'.'}],
                      [partial_sim_collection.solve_dict_list, {'label':"partial sweep", 'marker':'.'}]
                      ], 
                      ["eta_thermal_calc", ""], ["dT_htf_des", "C"], title="G3P3")
    
    plt.show(block = True)






    

# Main Script

if __name__ == "__main__":
    example_plot_solve_dict()