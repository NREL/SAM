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
import math

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
    tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\TSF_G3P3_collection_10_20240426_224925.csv"
    recomp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\recomp_G3P3_collection_10_20240426_223109.csv"
    partial_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\partial_G3P3_collection_10_20240426_204607.csv"
    htrbp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\htrbp_G3P3_collection_10_20240427_205838.csv"

    # Load smaller files
    if False:
        tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\reduced\TSF_G3P3_collection_5_20240426_163601.csv"
        recomp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\reduced\recomp_G3P3_collection_5_20240426_163437.csv"
        partial_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\reduced\partial_G3P3_collection_5_20240426_162235.csv"
        htrbp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\reduced\htrbp_G3P3_collection_5_20240426_174143.csv"

    # Load huge files
    if False:
        tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\mega\TSF_G3P3_collection_20_20240428_214905.csv"
        recomp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\mega\recomp_G3P3_collection_20_20240428_172705.csv"
        partial_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\mega\partial_G3P3_collection_20_20240428_044506.csv"

    # Test Pareto
    if False:
        print("Opening tsf...")
        tsf_sim_collection = sco2_solve.C_sco2_sim_result_collection()
        tsf_sim_collection.open_csv(tsf_filename)
        print("TSF opened")

        validate_costs(tsf_sim_collection.old_result_dict)

        X_label = "eta_thermal_calc"
        Y_label = "T_htf_cold_des"
        Z_label = "bypass_frac"

        X_unit = ""
        Y_unit = "C"
        Z_unit = ""
        tsf_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, X_label, Y_label, True, False)

        x = ""

    # Test Split
    if False:
        print("Opening partial...")
        partial_sim_collection = sco2_solve.C_sco2_sim_result_collection()
        partial_sim_collection.open_csv(partial_filename)
        print("Partial opened")

        print("Opening recomp...")
        recomp_sim_collection = sco2_solve.C_sco2_sim_result_collection()
        recomp_sim_collection.open_csv(recomp_filename)
        print("Recomp open")

        print("Opening tsf...")
        tsf_sim_collection = sco2_solve.C_sco2_sim_result_collection()
        tsf_sim_collection.open_csv(tsf_filename)
        print("TSF opened")

        design_tools.combine_common_runs([partial_sim_collection.old_result_dict, recomp_sim_collection.old_result_dict, tsf_sim_collection.old_result_dict])


        x = design_tools.split_by_key_UDPATED(htrbp_sim_collection.old_result_dict, "config_name")
        x.append(*design_tools.split_by_key_UDPATED(recomp_sim_collection.old_result_dict, "config_name"))
        simple_dicts = []
        for dict in x:
            if(dict["config_name"][0] == simple_legend_label):
                simple_dicts.append(dict)
        simple_combined_dict = design_tools.combine_dicts(simple_dicts)

        d = 0

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

    # Validate Cost Calculations
    if False:
        htrbp_cost_error = validate_costs(htrbp_sim_collection.old_result_dict)
        recomp_cost_error = validate_costs(recomp_sim_collection.old_result_dict)
        tsf_cost_error = validate_costs(tsf_sim_collection.old_result_dict)
        partial_cost_error = validate_costs(partial_sim_collection.old_result_dict)

    # Split Dicts by 'Actual' config name

    print("Splitting by config type...")

    # HTR BP (only comes from htr bp file)
    htrbp_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict],
                                                            "config_name", "htr bp")

    # Recompression (comes from recomp and htr bp)
    recomp_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict, 
                                                            recomp_sim_collection.old_result_dict],  
                                                            "config_name", "recompression")

    # Simple (from recomp and htr bp)
    simple_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict, 
                                                            recomp_sim_collection.old_result_dict],  
                                                            "config_name", "simple")

    # Simple w/ htr bypass (from htr bp only)
    simple_htrbp_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict],
                                                            "config_name", "simple split flow bypass")
    
    # Partial (only comes from partial file)
    partial_compiled_dict = design_tools.combine_dict_by_key([partial_sim_collection.old_result_dict],
                                                            "config_name", "partial")

    # Partial Intercooling (only comes from partial file)
    partial_ic_compiled_dict = design_tools.combine_dict_by_key([partial_sim_collection.old_result_dict],
                                                            "config_name", "partial intercooling")

    # Variables to Display
    ETA_label = ["eta_thermal_calc", "", "Cycle Thermal Efficiency"]
    T_HTF_label = ["T_htf_cold_des", "C", "HTF Outlet Temperature"]
    COST_label = ["cycle_cost", "M$", "Cycle Cost"]

    
    # Create T HTF Pareto Fronts
    print("Forming T HTF pareto fronts...")
    htrbp_T_HTF_pareto_dict = design_tools.get_pareto_dict(htrbp_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)
    recomp_T_HTF_pareto_dict = design_tools.get_pareto_dict(recomp_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)
    tsf_T_HTF_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)
    partial_T_HTF_pareto_dict = design_tools.get_pareto_dict(partial_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)

    # Create Cost Pareto Fronts
    print("Forming cost pareto fronts...")
    htrbp_cost_pareto_dict = design_tools.get_pareto_dict(htrbp_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)
    recomp_cost_pareto_dict = design_tools.get_pareto_dict(recomp_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)
    tsf_cost_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)
    partial_cost_pareto_dict = design_tools.get_pareto_dict(partial_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)

    # Create Cycle Split T HTF Pareto
    print("Forming split cycle T HTF pareto fronts...")
    htrbp_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(htrbp_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    recomp_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(recomp_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    simple_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(simple_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    simple_htrbp_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(simple_htrbp_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    partial_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(partial_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    partial_ic_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(partial_ic_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)

    # Create Cycle Split Cost Pareto
    print("Forming split cycle cost pareto fronts...")
    htrbp_compiled_cost_pareto_dict = design_tools.get_pareto_dict(htrbp_compiled_dict, ETA_label[0], COST_label[0], True, False)
    recomp_compiled_cost_pareto_dict = design_tools.get_pareto_dict(recomp_compiled_dict, ETA_label[0], COST_label[0], True, False)
    simple_compiled_cost_pareto_dict = design_tools.get_pareto_dict(simple_compiled_dict, ETA_label[0], COST_label[0], True, False)
    simple_htrbp_compiled_cost_pareto_dict = design_tools.get_pareto_dict(simple_htrbp_compiled_dict, ETA_label[0], COST_label[0], True, False)
    partial_compiled_cost_pareto_dict = design_tools.get_pareto_dict(partial_compiled_dict, ETA_label[0], COST_label[0], True, False)
    partial_ic_compiled_cost_pareto_dict = design_tools.get_pareto_dict(partial_ic_compiled_dict, ETA_label[0], COST_label[0], True, False)

    # Create Cycle Split Cost vs T HTF Pareto
    print("Forming split cycle cost vs t htf pareto fronts...")
    htrbp_compiled_cost_htf_pareto_dict = design_tools.get_pareto_dict(htrbp_compiled_dict, T_HTF_label[0], COST_label[0], False, False)
    recomp_compiled_cost_htf_pareto_dict = design_tools.get_pareto_dict(recomp_compiled_dict, T_HTF_label[0], COST_label[0], False, False)
    simple_compiled_cost_htf_pareto_dict = design_tools.get_pareto_dict(simple_compiled_dict, T_HTF_label[0], COST_label[0], False, False)
    simple_htrbp_compiled_cost_htf_pareto_dict = design_tools.get_pareto_dict(simple_htrbp_compiled_dict, T_HTF_label[0], COST_label[0], False, False)
    partial_compiled_cost_htf_pareto_dict = design_tools.get_pareto_dict(partial_compiled_dict, T_HTF_label[0], COST_label[0], False, False)
    partial_ic_compiled_cost_htf_pareto_dict = design_tools.get_pareto_dict(partial_ic_compiled_dict, T_HTF_label[0], COST_label[0], False, False)
    tsf_cost_htf_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, T_HTF_label[0], COST_label[0], False, False)

    # Plot
    print("Plotting...")
    figure_size=[8,6]
    htrbp_legend_label = "recompression w/ htr bypass"
    recomp_legend_label = "recompression"
    simple_legend_label = "simple"
    simple_bp_legend_label = "simple w/ bypass"
    partial_legend_label = "partial cooling"
    partial_ic_legend_label = "simple intercooling"
    tsf_legend_label = "turbine split flow"

    # Subsets (ETA X Axis)
    if True:
        # Compiled Pareto (Temp vs ETA)
        design_tools.plot_scatter_pts([[simple_compiled_T_HTF_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_T_HTF_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                        [recomp_compiled_T_HTF_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_compiled_T_HTF_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_T_HTF_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, T_HTF_label, title="G3P3 Pareto w/ Subset Configs", figure_size=figure_size)

        # Compiled Pareto (Cost vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_cost_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_cost_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                        [recomp_compiled_cost_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_cost_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_compiled_cost_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_cost_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [tsf_cost_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, COST_label, title="G3P3 Cost Pareto w/ Subset Configs", figure_size=figure_size)

        # Compiled All Data (Temp vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                        [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],          
                        [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, T_HTF_label, title="G3P3 w/ Subset Configs", figure_size=figure_size)

        # Compiled All Data (Cost vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                        [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, COST_label, title="G3P3 Cost w/ Subset Configs", figure_size=figure_size)

    # Cost vs T HTF
    if True:
            # Sweep
            design_tools.plot_scatter_pts([
                        [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                        [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_dict, {'label':"htr bp sweep", 'marker':'.'}],
                        [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        T_HTF_label, COST_label, title="G3P3 w/ Subset Configs", figure_size=figure_size)
            # Pareto
            design_tools.plot_scatter_pts([ 
                            [simple_compiled_cost_htf_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                            [simple_htrbp_compiled_cost_htf_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                            [recomp_compiled_cost_htf_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                            [htrbp_compiled_cost_htf_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                            [partial_ic_compiled_cost_htf_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                            [partial_compiled_cost_htf_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                            [tsf_cost_htf_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                            ], 
                            T_HTF_label, COST_label, title="G3P3 Pareto w/ Subset Configs", figure_size=figure_size)

    # Combine all paretos
    if True:
        # Combine the three types of pareto front
        htrbp_ult_pareto_dict = design_tools.combine_dicts([htrbp_compiled_T_HTF_pareto_dict, htrbp_compiled_cost_pareto_dict, htrbp_compiled_cost_htf_pareto_dict])
        recomp_ult_pareto_dict = design_tools.combine_dicts([recomp_compiled_T_HTF_pareto_dict, recomp_compiled_cost_pareto_dict, recomp_compiled_cost_htf_pareto_dict])
        simple_ult_pareto_dict = design_tools.combine_dicts([simple_compiled_T_HTF_pareto_dict, simple_compiled_cost_pareto_dict, simple_compiled_cost_htf_pareto_dict])
        simple_htrbp_ult_pareto_dict = design_tools.combine_dicts([simple_htrbp_compiled_T_HTF_pareto_dict, simple_htrbp_compiled_cost_pareto_dict, simple_htrbp_compiled_cost_htf_pareto_dict])
        partial_ult_pareto_dict = design_tools.combine_dicts([partial_compiled_T_HTF_pareto_dict, partial_compiled_cost_pareto_dict, partial_compiled_cost_htf_pareto_dict])
        partial_ic_ult_pareto_dict = design_tools.combine_dicts([partial_ic_compiled_T_HTF_pareto_dict, partial_ic_compiled_cost_pareto_dict, partial_ic_compiled_cost_htf_pareto_dict])
        tsf_ult_pareto_dict = design_tools.combine_dicts([tsf_T_HTF_pareto_dict, tsf_cost_pareto_dict, tsf_cost_htf_pareto_dict])

        design_tools.plot_scatter_pts([
                        [simple_ult_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_ult_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                        [recomp_ult_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_ult_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_ult_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_ult_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [tsf_ult_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, T_HTF_label, title="G3P3 All Paretos", figure_size=figure_size)

        # HTRBP Ult
        design_tools.plot_scatter_pts([
                            [htrbp_ult_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                            ], 
                            ETA_label, T_HTF_label, title="HTRBP Ult Pareto", figure_size=figure_size)

        # HTRBP T HTF
        design_tools.plot_scatter_pts([
                            [htrbp_compiled_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                            ], 
                            ETA_label, T_HTF_label, title="HTRBP T HTF Pareto", figure_size=figure_size)
        
        # HTRBP Cost
        design_tools.plot_scatter_pts([
                            [htrbp_compiled_cost_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                            ], 
                            ETA_label, T_HTF_label, title="HTRBP Cost Pareto", figure_size=figure_size)
        
        # HTRBP COST T
        design_tools.plot_scatter_pts([
                            [htrbp_compiled_cost_htf_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                            ], 
                            ETA_label, T_HTF_label, title="HTRBP Cost vs HTF Pareto", figure_size=figure_size)
        
        # HTRBP COST T
        design_tools.plot_scatter_pts([
                            [htrbp_compiled_T_HTF_pareto_dict, {'label':"T HTF vs ETA", 'marker':'.'}],
                            [htrbp_compiled_cost_pareto_dict, {'label':"Cost vs ETA", 'marker':'.'}],
                            [htrbp_compiled_cost_htf_pareto_dict, {'label':"T HTF vs Cost", 'marker':'.'}],
                            ], 
                            ETA_label, T_HTF_label, title="HTRBP Cost vs HTF Pareto", figure_size=figure_size)

    # Overlap paretos
    if False:
        compare_key_list = ["eta_thermal_calc", "T_htf_cold_des", "cycle_cost"]
        htrbp_overlap_pareto = design_tools.combine_common_runs([htrbp_compiled_T_HTF_pareto_dict, htrbp_compiled_cost_pareto_dict, htrbp_compiled_cost_htf_pareto_dict], compare_key_list)
        recomp_overlap_pareto_dict = design_tools.combine_common_runs([recomp_compiled_T_HTF_pareto_dict, recomp_compiled_cost_pareto_dict, recomp_compiled_cost_htf_pareto_dict], compare_key_list)
        simple_overlap_pareto_dict = design_tools.combine_common_runs([simple_compiled_T_HTF_pareto_dict, simple_compiled_cost_pareto_dict, simple_compiled_cost_htf_pareto_dict], compare_key_list)
        simple_htrbp_overlap_pareto_dict = design_tools.combine_common_runs([simple_htrbp_compiled_T_HTF_pareto_dict, simple_htrbp_compiled_cost_pareto_dict, simple_htrbp_compiled_cost_htf_pareto_dict], compare_key_list)
        partial_overlap_pareto_dict = design_tools.combine_common_runs([partial_compiled_T_HTF_pareto_dict, partial_compiled_cost_pareto_dict, partial_compiled_cost_htf_pareto_dict], compare_key_list)
        partial_ic_overlap_pareto_dict = design_tools.combine_common_runs([partial_ic_compiled_T_HTF_pareto_dict, partial_ic_compiled_cost_pareto_dict, partial_ic_compiled_cost_htf_pareto_dict], compare_key_list)
        tsf_overlap_pareto_dict = design_tools.combine_common_runs([tsf_T_HTF_pareto_dict, tsf_cost_pareto_dict, tsf_cost_htf_pareto_dict], compare_key_list)

        test = ""

    # Highlight Data Points
    if True:
        simple_pt_dict, simple_bp_pt_dict, recomp_pt_dict, htrbp_pt_dict, simple_ic_pt_dict, partial_pt_dict, tsf_pt_dict = load_highlight_data()
        colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
        sweep_alpha = 1
        highlight_pt_size = 500
        # Compiled Pareto (Temp vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_T_HTF_pareto_dict, {'label':simple_legend_label, 'alpha':sweep_alpha, 'c':colors[0], 'marker':'.'}],
                        [simple_pt_dict, {'label':simple_legend_label, 'c':colors[0], 's':highlight_pt_size, 'marker':'1'}],

                        [simple_htrbp_compiled_T_HTF_pareto_dict, {'label':simple_bp_legend_label, 'alpha':sweep_alpha, 'c':colors[1], 'marker':'.'}],
                        [simple_bp_pt_dict, {'label':simple_bp_legend_label, 's':highlight_pt_size, 'c':colors[1], 'marker':'1'}],

                        [recomp_compiled_T_HTF_pareto_dict, {'label':recomp_legend_label, 'alpha':sweep_alpha, 'c':colors[2], 'marker':'.'}],
                        [recomp_pt_dict, {'label':recomp_legend_label, 's':highlight_pt_size, 'c':colors[2], 'marker':'1'}],

                        [htrbp_compiled_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'alpha':sweep_alpha, 'c':colors[3], 'marker':'.'}],
                        [htrbp_pt_dict, {'label':htrbp_legend_label, 's':highlight_pt_size, 'c':colors[3], 'marker':'1'}],

                        [partial_ic_compiled_T_HTF_pareto_dict, {'label':partial_ic_legend_label, 'alpha':sweep_alpha, 'c':colors[4], 'marker':'.'}],
                        [simple_ic_pt_dict, {'label':partial_ic_legend_label, 's':highlight_pt_size, 'c':colors[4], 'marker':'1'}],

                        [partial_compiled_T_HTF_pareto_dict, {'label':partial_legend_label, 'alpha':sweep_alpha, 'c':colors[5], 'marker':'.'}],
                        [partial_pt_dict, {'label':partial_legend_label, 's':highlight_pt_size, 'c':colors[5], 'marker':'1'}],

                        [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'alpha':sweep_alpha, 'c':colors[6], 'marker':'.'}],
                        [tsf_pt_dict, {'label':tsf_legend_label, 's':highlight_pt_size, 'c':colors[6], 'marker':'1'}],
                        ], 
                        ETA_label, T_HTF_label, title="G3P3 Pareto w/ Subset Configs", figure_size=figure_size)

        # Compiled Pareto (Cost vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_cost_pareto_dict, {'label':simple_legend_label, 'alpha':sweep_alpha, 'c':colors[0], 'marker':'.'}],
                        [simple_pt_dict, {'label':simple_legend_label, 'c':colors[0], 's':highlight_pt_size, 'marker':'1'}],

                        [simple_htrbp_compiled_cost_pareto_dict, {'label':simple_bp_legend_label, 'alpha':sweep_alpha, 'c':colors[1], 'marker':'.'}],
                        [simple_bp_pt_dict, {'label':simple_bp_legend_label, 's':highlight_pt_size, 'c':colors[1], 'marker':'1'}],

                        [recomp_compiled_cost_pareto_dict, {'label':recomp_legend_label, 'alpha':sweep_alpha, 'c':colors[2], 'marker':'.'}],
                        [recomp_pt_dict, {'label':recomp_legend_label, 's':highlight_pt_size, 'c':colors[2], 'marker':'1'}],

                        [htrbp_compiled_cost_pareto_dict, {'label':htrbp_legend_label, 'alpha':sweep_alpha, 'c':colors[3], 'marker':'.'}],
                        [htrbp_pt_dict, {'label':htrbp_legend_label, 's':highlight_pt_size, 'c':colors[3], 'marker':'1'}],

                        [partial_ic_compiled_cost_pareto_dict, {'label':partial_ic_legend_label, 'alpha':sweep_alpha, 'c':colors[4], 'marker':'.'}],
                        [simple_ic_pt_dict, {'label':partial_ic_legend_label, 's':highlight_pt_size, 'c':colors[4], 'marker':'1'}],

                        [partial_compiled_cost_pareto_dict, {'label':partial_legend_label, 'alpha':sweep_alpha, 'c':colors[5], 'marker':'.'}],
                        [partial_pt_dict, {'label':partial_legend_label, 's':highlight_pt_size, 'c':colors[5], 'marker':'1'}],

                        [tsf_cost_pareto_dict, {'label':tsf_legend_label, 'alpha':sweep_alpha, 'c':colors[6], 'marker':'.'}],
                        [tsf_pt_dict, {'label':tsf_legend_label, 's':highlight_pt_size, 'c':colors[6], 'marker':'1'}],
                        ], 
                        ETA_label, COST_label, title="G3P3 Pareto w/ Subset Configs", figure_size=figure_size)

        test = 0

    # Raw data (no subset splitting)
    if False:
        # Pareto (Temp vs ETA)
        design_tools.plot_scatter_pts([[tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_T_HTF_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [partial_T_HTF_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [htrbp_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, T_HTF_label, title="G3P3 Pareto", figure_size=figure_size)

        # Pareto (Cost vs ETA)
        design_tools.plot_scatter_pts([[tsf_cost_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_cost_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [partial_cost_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [htrbp_cost_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, COST_label, title="G3P3 Cost Pareto", figure_size=figure_size)

        # All Data (Temp vs ETA)
        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, T_HTF_label, title="G3P3", figure_size=figure_size)
        
        # All Data (Cost vs ETA)
        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, COST_label, title="G3P3 Cost", figure_size=figure_size)

    # Design Parameter Plots (raw data)
    if False:
        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'1'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'2'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'3'}]
                        ], 
                        ETA_label, T_HTF_label, ["recomp_frac", ""],title="G3P3 Recomp", figure_size=figure_size)

        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'1'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'2'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'3'}]
                        ], 
                        ETA_label, T_HTF_label, ["recup_total_UA_calculated", "MW/K"],title="G3P3 Total UA", figure_size=figure_size)
        
        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'1'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'2'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'3'}]
                        ], 
                        ETA_label, T_HTF_label, ["is_PR_fixed", "MPa"],title="G3P3 Min Pressure", figure_size=figure_size)

        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'1'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'2'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'3'}]
                        ], 
                        ETA_label, T_HTF_label, ["T_state_points_5_0", "C"],title="G3P3 Turbine Inlet Temp", figure_size=figure_size)

        design_tools.plot_scatter_pts([[tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}], 
                        [recomp_sim_collection.old_result_dict, {'label':recomp_legend_label, 'marker':'1'}],
                        [partial_sim_collection.old_result_dict, {'label':partial_legend_label, 'marker':'2'}],
                        [htrbp_sim_collection.old_result_dict, {'label':htrbp_legend_label, 'marker':'3'}]
                        ], 
                        ETA_label, T_HTF_label, ["T_co2_PHX_in", "C"],title="G3P3 PHX sco2 Inlet Temp", figure_size=figure_size)

    # Abstract specific plots
    if True:
        fig_abstract, (ax1_abstract, ax2_abstract) = plt.subplots(1,2)
        
        # Compiled All Data (Temp vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                        [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],          
                        [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, T_HTF_label, ax=ax1_abstract, show_legend=False)

        # Compiled All Data (Cost vs ETA)
        design_tools.plot_scatter_pts([
                        [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                        [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                        [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                        [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                        [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                        [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],
                        [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                        ], 
                        ETA_label, COST_label, ax=ax2_abstract, show_legend=True, legend_loc='upper right')

    plt.tight_layout()
    plt.show(block = True)

def load_highlight_data():
    simple_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\simple_data_point.txt"
    simple_bp_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\simple_bp_data_point.txt"
    recomp_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\recomp_data_point.txt"
    htrbp_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\htrbp_data_point.txt"
    simple_ic_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\simple_ic_data_point.txt"
    partial_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\partial_data_point.txt"
    tsf_pt_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\Highlight Points\tsf_data_point.txt"
    
    file_name_list = [simple_pt_filename, 
                      simple_bp_pt_filename,
                      recomp_pt_filename,
                      htrbp_pt_filename,
                      simple_ic_pt_filename,
                      partial_pt_filename,
                      tsf_pt_filename]

    dict_list = []
    for file_name in file_name_list:
        test_dict = design_tools.get_dict_from_file_w_STRING(file_name)
        real_dict = {}
        for key in test_dict:
            real_dict[key] = [test_dict[key][0],] 
        dict_list.append(real_dict)

    return dict_list

def validate_costs(result_dict):
    cost_equ_keys = ["mc_cost_equipment", "rc_cost_equipment", "pc_cost_equipment", "c_tot_cost_equip", "t_cost_equipment", "t2_cost_equipment", "LTR_cost_equipment", "HTR_cost_equipment",
                         "PHX_cost_equipment", "BPX_cost_equipment", "mc_cooler_cost_equipment", "pc_cooler_cost_equipment"]
    cost_erected_keys = ["mc_cost_bare_erected", "rc_cost_bare_erected", "pc_cost_bare_erected", "t_cost_bare_erected", "t2_cost_bare_erected", "LTR_cost_bare_erected", "HTR_cost_bare_erected",
                         "PHX_cost_bare_erected", "BPX_cost_bare_erected", "mc_cooler_cost_bare_erected", "pc_cooler_cost_bare_erected", "piping_inventory_etc_cost"]
    
    error_id_list = []
    NVal = len(result_dict['htf'])
    for i in range(NVal):
        total_equip_cost = 0
        total_erected_cost = 0
        for key in cost_equ_keys:
            if key in result_dict:
                val = result_dict[key][i]
                if(isinstance(val, str) == False and math.isnan(val) == False):
                    total_equip_cost += val
        for key in cost_erected_keys:
            if key in result_dict:
                val = result_dict[key][i]
                if(isinstance(val, str) == False and math.isnan(val) == False):
                    total_erected_cost += val
        
        cost_reported = result_dict['cycle_cost'][i]
        cost_calc = total_erected_cost

        if(cost_reported != cost_calc):
            error_id_list.append(i)

    return error_id_list
    
def test_combine_paretos():

    tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\TSF_G3P3_collection_10_20240426_224925.csv"
    print("Opening tsf...")
    tsf_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    tsf_sim_collection.open_csv(tsf_filename)
    print("TSF opened")

    # Variables to Display
    ETA_label = ["eta_thermal_calc", "", "Cycle Thermal Efficiency"]
    T_HTF_label = ["T_htf_cold_des", "C", "HTF Outlet Temperature"]
    COST_label = ["cycle_cost", "M$", "Cycle Cost"]

    # Create T HTF Pareto Fronts
    print("Forming T HTF pareto fronts...")
    tsf_T_HTF_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)

    # Create Cost Pareto Fronts
    print("Forming cost pareto fronts...")
    tsf_cost_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)

    # Create Cycle Split Cost vs T HTF Pareto
    tsf_cost_htf_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, T_HTF_label[0], COST_label[0], False, False)

    common_dict = design_tools.combine_common_runs([tsf_T_HTF_pareto_dict, tsf_cost_pareto_dict, tsf_cost_htf_pareto_dict], ["eta_thermal_calc", "T_htf_cold_des", "cycle_cost"])

def solarpaces_2024_abstract():
    tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\TSF_G3P3_collection_10_20240426_224925.csv"
    recomp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\recomp_G3P3_collection_10_20240426_223109.csv"
    partial_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\partial_G3P3_collection_10_20240426_204607.csv"
    htrbp_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\htrbp_G3P3_collection_10_20240427_205838.csv"

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

    # Validate Cost Calculations
    if False:
        htrbp_cost_error = validate_costs(htrbp_sim_collection.old_result_dict)
        recomp_cost_error = validate_costs(recomp_sim_collection.old_result_dict)
        tsf_cost_error = validate_costs(tsf_sim_collection.old_result_dict)
        partial_cost_error = validate_costs(partial_sim_collection.old_result_dict)

    # Split Dicts by 'Actual' config name

    print("Splitting by config type...")

    # HTR BP (only comes from htr bp file)
    htrbp_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict],
                                                            "config_name", "htr bp")

    # Recompression (comes from recomp and htr bp)
    recomp_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict, 
                                                            recomp_sim_collection.old_result_dict],  
                                                            "config_name", "recompression")

    # Simple (from recomp and htr bp)
    simple_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict, 
                                                            recomp_sim_collection.old_result_dict],  
                                                            "config_name", "simple")

    # Simple w/ htr bypass (from htr bp only)
    simple_htrbp_compiled_dict = design_tools.combine_dict_by_key([htrbp_sim_collection.old_result_dict],
                                                            "config_name", "simple split flow bypass")
    
    # Partial (only comes from partial file)
    partial_compiled_dict = design_tools.combine_dict_by_key([partial_sim_collection.old_result_dict],
                                                            "config_name", "partial")

    # Partial Intercooling (only comes from partial file)
    partial_ic_compiled_dict = design_tools.combine_dict_by_key([partial_sim_collection.old_result_dict],
                                                            "config_name", "partial intercooling")

    # Variables to Display
    ETA_label = ["eta_thermal_calc", "", "Cycle Thermal Efficiency"]
    T_HTF_label = ["T_htf_cold_des", "C", "HTF Outlet Temperature"]
    COST_label = ["cycle_cost", "M$", "Cycle Cost"]

    # Variables to Display
    ETA_label = ["eta_thermal_calc", "", "Cycle Thermal Efficiency"]
    T_HTF_label = ["T_htf_cold_des", "C", "HTF Outlet Temperature"]
    COST_label = ["cycle_cost", "M$", "Cycle Cost"]

    # Create Cycle Split T HTF Pareto
    print("Forming split cycle T HTF pareto fronts...")
    htrbp_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(htrbp_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    recomp_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(recomp_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    simple_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(simple_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    simple_htrbp_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(simple_htrbp_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    partial_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(partial_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    partial_ic_compiled_T_HTF_pareto_dict = design_tools.get_pareto_dict(partial_ic_compiled_dict, ETA_label[0], T_HTF_label[0], True, False)
    tsf_T_HTF_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)

    # Create Cycle Split Cost Pareto
    print("Forming split cycle cost pareto fronts...")
    htrbp_compiled_cost_pareto_dict = design_tools.get_pareto_dict(htrbp_compiled_dict, ETA_label[0], COST_label[0], True, False)
    recomp_compiled_cost_pareto_dict = design_tools.get_pareto_dict(recomp_compiled_dict, ETA_label[0], COST_label[0], True, False)
    simple_compiled_cost_pareto_dict = design_tools.get_pareto_dict(simple_compiled_dict, ETA_label[0], COST_label[0], True, False)
    simple_htrbp_compiled_cost_pareto_dict = design_tools.get_pareto_dict(simple_htrbp_compiled_dict, ETA_label[0], COST_label[0], True, False)
    partial_compiled_cost_pareto_dict = design_tools.get_pareto_dict(partial_compiled_dict, ETA_label[0], COST_label[0], True, False)
    partial_ic_compiled_cost_pareto_dict = design_tools.get_pareto_dict(partial_ic_compiled_dict, ETA_label[0], COST_label[0], True, False)
    tsf_cost_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)

    # Plot
    print("Plotting...")
    htrbp_legend_label = "recompression w/ htr bypass"
    recomp_legend_label = "recompression"
    simple_legend_label = "simple"
    simple_bp_legend_label = "simple w/ bypass"
    partial_legend_label = "partial cooling"
    partial_ic_legend_label = "simple intercooling"
    tsf_legend_label = "turbine split flow"

    fig_width = 6 * 1.6
    fig_height = 2.2 * 1.6

    # Abstract specific plots
    fig_abstract, (ax1_abstract, ax2_abstract) = plt.subplots(1,2)
    fig_abstract.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([
                    [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                    [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],          
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract, show_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                    [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract, show_legend=True, legend_loc='upper right')
    ax2_abstract.set_ylim(0,70)
    plt.tight_layout()
    plt.rc('font', size=11) 


    # Abstract specific plots
    fig_abstract_pareto, (ax1_abstract_pareto, ax2_abstract_pareto) = plt.subplots(1,2)
    fig_abstract_pareto.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([
                    [simple_compiled_T_HTF_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_T_HTF_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                    [recomp_compiled_T_HTF_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_T_HTF_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_T_HTF_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],          
                    [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract_pareto, show_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [simple_compiled_cost_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_cost_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                    [recomp_compiled_cost_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_cost_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_cost_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_cost_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                    [tsf_cost_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract_pareto, show_legend=True, legend_loc='upper left')
    #ax2_abstract_pareto.set_ylim(0,70)
    plt.tight_layout()
    plt.rc('font', size=11) 

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([
                    [simple_compiled_T_HTF_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_T_HTF_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                    [recomp_compiled_T_HTF_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_T_HTF_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_T_HTF_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],          
                    [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, show_legend=True)

    # Combine All 4 Plots
    if True:
        fig_abstract_all, ((ax1_abstract_allsweep, ax2_abstract_allsweep), (ax1_abstract_allpareto, ax2_abstract_allpareto)) = plt.subplots(2,2)
        fig_abstract_all.set_size_inches(fig_width, fig_height * 2)

        # Sweep (Temp vs ETA)
        design_tools.plot_scatter_pts([
                    [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                    [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],          
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract_allsweep, show_legend=False)
        
        # Sweep (Cost vs ETA)
        design_tools.plot_scatter_pts([
                    [simple_compiled_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                    [recomp_compiled_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_dict, {'label':partial_legend_label, 'marker':'.'}],
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract_allsweep, show_legend=True, legend_loc='upper right')
        
        # Pareto (Temp vs ETA)
        design_tools.plot_scatter_pts([
                    [simple_compiled_T_HTF_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_T_HTF_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],             
                    [recomp_compiled_T_HTF_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_T_HTF_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_T_HTF_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_T_HTF_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],          
                    [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract_allpareto, show_legend=False)

        # Pareto (Cost vs ETA)
        design_tools.plot_scatter_pts([
                    [simple_compiled_cost_pareto_dict, {'label':simple_legend_label, 'marker':'.'}],
                    [simple_htrbp_compiled_cost_pareto_dict, {'label':simple_bp_legend_label, 'marker':'.'}],
                    [recomp_compiled_cost_pareto_dict, {'label':recomp_legend_label, 'marker':'.'}],
                    [htrbp_compiled_cost_pareto_dict, {'label':htrbp_legend_label, 'marker':'.'}],
                    [partial_ic_compiled_cost_pareto_dict, {'label':partial_ic_legend_label, 'marker':'.'}],
                    [partial_compiled_cost_pareto_dict, {'label':partial_legend_label, 'marker':'.'}],
                    [tsf_cost_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract_allpareto, show_legend=False, legend_loc='upper left')

        plt.tight_layout()
        plt.rc('font', size=11) 

    plt.show(block = True)

def asme_es_plots():
    tsf_filename = r"C:\Users\tbrown2\Desktop\sco2_python\G3P3\TSF_G3P3_collection_10_20240426_224925.csv"

    print("Opening tsf...")
    tsf_sim_collection = sco2_solve.C_sco2_sim_result_collection()
    tsf_sim_collection.open_csv(tsf_filename)
    print("TSF opened")

    # Convert Split Fraction
    if ("split_frac" in tsf_sim_collection.old_result_dict) == False:

        tsf_sim_collection.old_result_dict["split_frac"] = []
        for val in tsf_sim_collection.old_result_dict["is_turbine_split_ok"]:
            tsf_sim_collection.old_result_dict["split_frac"].append(val * -1)

    # Variables to Display
    ETA_label = ["eta_thermal_calc", "", "Cycle Thermal Efficiency"]
    T_HTF_label = ["T_htf_cold_des", "C", "HTF Outlet Temperature"]
    COST_label = ["cycle_cost", "M$", "Cycle Cost"]

    # Variables to Display
    ETA_label = ["eta_thermal_calc", "", "Cycle Thermal Efficiency"]
    T_HTF_label = ["T_htf_cold_des", "C", "HTF Outlet Temperature"]
    COST_label = ["cycle_cost", "M$", "Cycle Cost"]

    # Create Cycle Split T HTF Pareto
    print("Forming TSF T HTF pareto fronts...")
    tsf_T_HTF_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], T_HTF_label[0], True, False)

    # Create Cycle Split Cost Pareto
    print("Forming TSF cost pareto fronts...")
    tsf_cost_pareto_dict = design_tools.get_pareto_dict(tsf_sim_collection.old_result_dict, ETA_label[0], COST_label[0], True, False)

    

    # Plot
    print("Plotting...")
    tsf_legend_label = "turbine split flow"

    fig_width = 6 * 1.6
    fig_height = 2.2 * 1.6

    # All data
    fig_abstract, (ax1_abstract, ax2_abstract) = plt.subplots(1,2)
    fig_abstract.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([   
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract, show_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract, show_legend=False, legend_loc='upper right')
    plt.tight_layout()
    plt.rc('font', size=11) 


    # Pareto Fronts
    fig_abstract_pareto, (ax1_abstract_pareto, ax2_abstract_pareto) = plt.subplots(1,2)
    fig_abstract_pareto.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([       
                    [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract_pareto, show_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [tsf_cost_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract_pareto, show_legend=False, legend_loc='upper left')
    plt.tight_layout()
    plt.rc('font', size=11) 


    # All data (Split Fraction Z)
    fig_abstract_splitZ, axs_splitZ = plt.subplots(1,2)
    fig_abstract_splitZ.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([   
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, Z_info=["split_frac", "", "Turbine Split Fraction"], ax=axs_splitZ[0], show_legend=False, show_Z_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, Z_info=["split_frac", "", "Turbine Split Fraction"], ax=axs_splitZ[1], show_legend=False, legend_loc='upper right',
                    show_Z_legend=True)
    
    plt.tight_layout()
    plt.rc('font', size=11) 

    # All data (Total UA Z)
    d = ""
    fig_abstract_UAZ, axs_UAZ = plt.subplots(1,2)
    fig_abstract_UAZ.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([   
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, Z_info=["recup_total_UA_calculated", "MW/K", "Total Conductance"], ax=axs_UAZ[0], show_legend=False, show_Z_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, Z_info=["recup_total_UA_calculated", "MW/K", "Total Conductance"], ax=axs_UAZ[1], show_legend=False, legend_loc='upper right',
                    show_Z_legend=True)
    
    plt.tight_layout()
    plt.rc('font', size=11) 

    # All data (Min Pressure Z)
    fig_abstract_presZ, axs_presZ = plt.subplots(1,2)
    fig_abstract_presZ.set_size_inches(fig_width, fig_height)

    # Compiled All Data (Temp vs ETA)
    design_tools.plot_scatter_pts([   
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, Z_info=["P_comp_in", "MPa", "Minimum Pressure"], ax=axs_presZ[0], show_legend=False, show_Z_legend=False)

    # Compiled All Data (Cost vs ETA)
    design_tools.plot_scatter_pts([
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, Z_info=["P_comp_in", "MPa", "Minimum Pressure"], ax=axs_presZ[1], show_legend=False, legend_loc='upper right',
                    show_Z_legend=True)
    
    plt.tight_layout()
    plt.rc('font', size=11) 

    # Combine All 4 Plots
    if False:
        fig_abstract_all, ((ax1_abstract_allsweep, ax2_abstract_allsweep), (ax1_abstract_allpareto, ax2_abstract_allpareto)) = plt.subplots(2,2)
        fig_abstract_all.set_size_inches(fig_width, fig_height * 2)

        # Sweep (Temp vs ETA)
        design_tools.plot_scatter_pts([
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract_allsweep, show_legend=False)
        
        # Sweep (Cost vs ETA)
        design_tools.plot_scatter_pts([
                    [tsf_sim_collection.old_result_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract_allsweep, show_legend=True, legend_loc='upper right')
        
        # Pareto (Temp vs ETA)
        design_tools.plot_scatter_pts([        
                    [tsf_T_HTF_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, T_HTF_label, ax=ax1_abstract_allpareto, show_legend=False)

        # Pareto (Cost vs ETA)
        design_tools.plot_scatter_pts([
                    [tsf_cost_pareto_dict, {'label':tsf_legend_label, 'marker':'.'}]
                    ], 
                    ETA_label, COST_label, ax=ax2_abstract_allpareto, show_legend=False, legend_loc='upper left')

        plt.tight_layout()
        plt.rc('font', size=11) 

    plt.show(block = True)

# Main Script

if __name__ == "__main__":
    solarpaces_2024_abstract()
    #asme_es_plots()
    #example_plot_solve_dict()