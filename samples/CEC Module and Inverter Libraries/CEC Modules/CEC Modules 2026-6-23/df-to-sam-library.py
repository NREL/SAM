import pandas as pd

filename_date = '2026-6-23'
df =  pd.read_csv('cec_modules_params_PV_Module_List_Full_Data_ADA_2026-6-23.csv')

sam_library_df = df
sam_library_df = sam_library_df.rename(columns={'a_py': 'a_ref', 'Il_py': 'I_L_ref', 'Io_py': 'I_o_ref', 'Rs_py': 'R_s', 'Rsh_py': 'R_sh_ref', 'Adj_py': 'Adjust'})
sam_library_df.insert(0, "Name", sam_library_df['Manufacturer'].astype(str) + " " + sam_library_df['Model Number'].astype(str))
sam_library_df = sam_library_df.drop(columns=['Model Number', 'd_Isc', 'd_Imp', 'd_Vmp', 'd_Pmp', 'Error'])

# SAM version and library generation date for version tracking in SAM
sam_library_df['Version'] = '2025.4.16'
sam_library_df['Date'] = '2/16/2026'

headers = { "Name": ["Units", "[0]"],
    "Manufacturer": ["", "lib_manufacturer"],
    "Technology": ["", "cec_material"],
    "Bifacial": ["", "lib_is_bifacial"],
    "STC": ["", ""],
    "PTC": ["", ""],
    "A_c": ["m2", "cec_area"],
    "Length": ["m", "lib_length"],
    "Width": ["m", "lib_width"],
    "N_s": ["", "cec_n_s"],
    "I_sc_ref": ["A", "cec_i_sc_ref"],
    "V_oc_ref": ["V", "cec_v_oc_ref"],
    "I_mp_ref": ["A", "cec_i_mp_ref"],
    "V_mp_ref": ["V", "cec_v_mp_ref"],
    "alpha_sc": ["A/K", "cec_alpha_sc"],
    "beta_oc": ["V/K", "cec_beta_oc"],
    "T_NOCT": ["C", "cec_t_noct"],
    "a_ref": ["V", "cec_a_ref"],
    "I_L_ref": ["A", "cec_i_l_ref"],
    "I_o_ref": ["A", "cec_i_o_ref"],
    "R_s": ["Ohm", "cec_r_s"],
    "R_sh_ref": ["Ohm", "cec_r_sh_ref"],
    "Adjust": ["%", "cec_adjust"],
    "gamma_pmp": ["%/K", "cec_gamma_pmp"],
    "BIPV": ["", ""],
    "Version": ["", ""],
    "Date": ["", ""]}

header_df = pd.DataFrame.from_dict(headers)
sam_library_df = pd.concat([header_df, sam_library_df])
sam_library_df.to_csv(f"CEC Modules {filename_date}.csv", index=False)