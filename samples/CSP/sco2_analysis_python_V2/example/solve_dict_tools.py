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
import design_point_tools as design_tools

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
    tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\TSF_G3P3_collection20240422_235632.csv"
    recomp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\recomp_G3P3_collection20240422_233921.csv"
    partial_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\partial_G3P3_collection20240422_220557.csv"
    htrbp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\htrbp_G3P3_collection20240424_072851.csv"

    print("Opening htrbp...")
    htrbp_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    htrbp_sim_collection.open_csv(htrbp_filename)
    print("HTR BP opened")

    print("Opening recomp...")
    recomp_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    recomp_sim_collection.open_csv(recomp_filename)
    print("Recomp open")

    print("Opening tsf...")
    tsf_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    tsf_sim_collection.open_csv(tsf_filename)
    print("TSF opened")

    print("Opening partial...")
    partial_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    partial_sim_collection.open_csv(partial_filename)
    print("Partial opened")


    # Variables to Display
    X_label = "eta_thermal_calc"
    Y_label = "T_htf_cold_des"
    Z_label = "bypass_frac"

    X_unit = ""
    Y_unit = "C"
    Z_unit = ""

    print("Forming pareto fronts...")

    # Create Pareto Fronts
    htrbp_pareto_dict = design_tools.get_pareto_front_from_dict(htrbp_sim_collection.old_result_dict, X_label, Y_label, True, False)
    recomp_pareto_dict = design_tools.get_pareto_front_from_dict(recomp_sim_collection.old_result_dict, X_label, Y_label, True, False)
    tsf_pareto_dict = design_tools.get_pareto_front_from_dict(tsf_sim_collection.old_result_dict, X_label, Y_label, True, False)
    partial_pareto_dict = design_tools.get_pareto_front_from_dict(partial_sim_collection.old_result_dict, X_label, Y_label, True, False)



    print("Plotting...")

    design_tools.plot_scatter_pts([[tsf_pareto_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_pareto_dict, {'label':"recomp sweep", 'marker':'.'}],
                      [partial_pareto_dict, {'label':"partial sweep", 'marker':'.'}],
                      [htrbp_pareto_dict, {'label':"htrbp sweep", 'marker':'.'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], title="G3P3 Pareto")

    design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.old_result_dict, {'label':"recomp sweep", 'marker':'.'}],
                      [partial_sim_collection.old_result_dict, {'label':"partial sweep", 'marker':'.'}],
                      [htrbp_sim_collection.old_result_dict, {'label':"htrbp sweep", 'marker':'.'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], title="G3P3")
    
    design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.old_result_dict, {'label':"recomp sweep", 'marker':'1'}],
                      [partial_sim_collection.old_result_dict, {'label':"partial sweep", 'marker':'2'}],
                      [htrbp_sim_collection.old_result_dict, {'label':"htrbp sweep", 'marker':'3'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], ["recomp_frac", ""],title="G3P3 Recomp")

    design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.old_result_dict, {'label':"recomp sweep", 'marker':'1'}],
                      [partial_sim_collection.old_result_dict, {'label':"partial sweep", 'marker':'2'}],
                      [htrbp_sim_collection.old_result_dict, {'label':"htrbp sweep", 'marker':'3'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], ["recup_total_UA_calculated", "MW/K"],title="G3P3 Total UA")
    
    design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.old_result_dict, {'label':"recomp sweep", 'marker':'1'}],
                      [partial_sim_collection.old_result_dict, {'label':"partial sweep", 'marker':'2'}],
                      [htrbp_sim_collection.old_result_dict, {'label':"htrbp sweep", 'marker':'3'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], ["is_PR_fixed", "MPa"],title="G3P3 Min Pressure")

    design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.old_result_dict, {'label':"recomp sweep", 'marker':'1'}],
                      [partial_sim_collection.old_result_dict, {'label':"partial sweep", 'marker':'2'}],
                      [htrbp_sim_collection.old_result_dict, {'label':"htrbp sweep", 'marker':'3'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], ["T_state_points_5_0", "C"],title="G3P3 Turbine Inlet Temp")

    design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':"tsf sweep", 'marker':'.'}], 
                      [recomp_sim_collection.old_result_dict, {'label':"recomp sweep", 'marker':'1'}],
                      [partial_sim_collection.old_result_dict, {'label':"partial sweep", 'marker':'2'}],
                      [htrbp_sim_collection.old_result_dict, {'label':"htrbp sweep", 'marker':'3'}]
                      ], 
                      ["eta_thermal_calc", ""], ["T_htf_cold_des", "C"], ["T_co2_PHX_in", "C"],title="G3P3 PHX sco2 Inlet Temp")


    plt.show(block = True)






    

# Main Script

if __name__ == "__main__":
    example_plot_solve_dict()