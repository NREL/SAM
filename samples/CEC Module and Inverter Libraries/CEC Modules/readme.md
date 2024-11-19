# CEC Module Library

This document explains how to build the SAM CEC module library file from the CEC list of eligible PV modules.

## Instructions

> **Note:** The CEC changes the format and adds information periodically, so these instructions may need to be updated.

### Step 1: Download full list of PV modules from CEC website

1. Create a new working folder for this version of the module library in [SAM/Samples/CEC Modlue and Inverter Libraries/CEC Modules](https://github.com/NREL/SAM/tree/develop/samples/CEC%20Module%20and%20Inverter%20Libraries/CEC%20Modules), like `CEC Modules yyyy-m-d` with today's date in the folder name.

2. Go to https://solarequipment.energy.ca.gov/Home/PVModuleList and click the **Download Excel file** link to the working folder.

3. Change the Excel file name with the last CEC update date (from Row 2 in the workbook) in the file name `PV_Module_List_Full_Data_ADA-yyyy-m-d.xlsx`.

### Step 2: Manually process CEC xlsx file

This step could be automated in Excel, but this process is a good excuse to do a visual review of the file.

1. Open file in Excel.

2. Save as Unicode UTF-8 CSV to preserve Greek characters used in column headings.

3. Close Excel.

3. Open CSV file in text editor.

4. Delete all rows up to column headings row (begins with "Manufacturer").

5. Delete soft line breaks LF `\n` in header rows so that there is one header row that begins with "Manufacturer" and ends with "Last Update." Show hidden characters in text editor to find single LF in first row. (Note remaining lines use CRLF.) (This step is new after CEC added in-cell linebreaks to some header cells after 3/1/2019.)

5. Search and replace LF followed by comma `\n,` with nothing to remove soft line breaks from columns (assumes Windows CRLF line endings). Repeat until all instances are found.

6. Search and replace comma followed by LF `,\n` with nothing to remove soft line breaks from columns (assumes Windows CRLF line endings). Repeat until all instances are found.

6. Search and replace space followed by LF ` \n` with comma `,` to fix some Excel cells with empty line in cell (see Trina modules).

6. Search and replace space followed by comma space ` ,` with comma `,`.

6. Search and replace comma followed by space `, ` with comma `,`.

5. Search and replace Greek characters in column headings row: alpha, beta, gamma. For example, replace βVoc with betaVoc. Should be one instance of gamma, and two instances each of alpha and beta.

5. Use regular expression search option to replace `[^\x00-\x7F]+` with empty string. This removes non-ASCII characters that cause problems in SAM UI (and may cause problems with LK reading data from file).

6. Save CSV file and close text editor.

7. Open CSV file in Excel.

8. Search and replace all commas `,` with nothing. (Can't do this in text editor because don't want to change column delimiters, only commas in some text fields.)

9. Delete units row, Row 2.

10. Save and close CSV file.

11. Open CSV file in text editor remove double quotes `"` by replacing them with nothing. (If the file is tab-delimited, replace tabs `\t` with  commas `,`.)

12. Save CSV file and close editor.

### Step 3: Run LK script to convert worksheet data into SAM library file

1. Start SAM and open the latest version of the Update CEC Modules script (`update_cec_modules_2023.lk` as of this writing).

2. Set `version` to the SAM version you are using to run the script.

3. Set `date` to today's date D/M/YYYY.

4. Run the script, and navigate to the CSV file you prepared in Step 2 above. It may take a while to run, on the order of 15-30 minutes for the databases in 2019.

### Step 4: Replace the CEC module library file in the SAM deploy folder

1. If a version of SAM you built from the SAM GitHub repo is open, close it.

2. Make sure your local copy of the repo is up to date (Develop branch for new release, Patch branch for update).

3. Go to `deploy/libraries` and replace the `CEC Modules.csv` file with the one you just created. Keep a copy of the library file in the sam-documentation repository so we have a record of it.

4. Start SAM (built from the Develop branch), create a detailed PV case, and make sure the new library works. **Spot check modules in the library to see if any I-V curve plots look wrong.**

### Step 5: Run test scripts

1. Start SAM and open cec_modules_test.sam.

2. Run `cec_modules_test.lk`.

3. Review the results in `cec_modules_test.csv`. 

4. Manually remove any modules with unrealistic performance ratios or simulation errors, and add to list of bad modules.

5. If all is well, run `test_script_ow.lk` to make sure default PV configurations work correctly.

## Notes

Watch for date formatting: If you set formatting of cells to general in Excel, it converts dates mm/dd/yyyy to a number.

I_o_ref is a very small number, so should be stored in exponential notation. **I-V curve will look square if I_o_ref gets stored as zero.**

Description mentions system voltage, e.g., 600 V or 1000 V for some modules.

Notes field indicates "ACPV" modules. For some modules, e.g. LG360A1C-A5, microinverter model is listed in Description. Could show Description and Notes in text box in SAM UI.

Family and Technology columns are similar, but use slightly different names for different cell types. Family includes three SolarCity modules called "HIT-SI Thin Film" that is shown as "Multi-c-SI" in the Technology column.

Temperature coefficients are given in %/degC rather than A/degC, V/degC, and W/degC.

In addition to Imp_ref and Vmp_ref, current and voltage is provided at "low" and "NOCT" conditions.

Mounting is either "Rack" or "BIPV" -- seems to duplicate "BIPV" column.

Type column is set to "Flat Plate" or blank for all modules.

Not all modules have Length and Width. 

"Nameplate Pmax" is available, could add to SAM library for sorting instead of PTC, even though may be different from Vmp * Imp shown in SAM UI.

Add text to SAM UI explaining that data comes from CEC with link?

## SAM library variable names mapped to spreadsheet names

Note unicode greek characters in spreadsheet for temperature coefficients.

SAM library, CEC spreadsheet name

Name, Manufacturer + Model Number
BIPV, BIPV
Date, ?? maybe date added to library?
T_NOCT, Average NOCT
A_c, A_c
N_s, N_s
I_sc_ref, Nameplate Isc
V_oc_ref, Nameplate Voc
I_mp_ref, Nameplate Ipmax
V_mp_ref, Nameplate Vpmax
alpha_sc, αIsc
beta_oc, βVoc
a_ref, calculate
I_L_ref, calculate
I_o_ref, calculate
R_s, calculate
R_sh_ref, calculate
Adjust, calculate
gamma_pmp, γPmax
Version
PTC, PTC
Technology, Technology

## Variables from CEC spreadsheet (Aug 15, 2018)

Manufacturer
Model Number
Description
BIPV
Nameplate Pmax
PTC
Notes
Family
A_c
N_s
Technology
Nameplate Isc
Nameplate Voc
Nameplate Ipmax
Nameplate Vpmax
Average NOCT
γPmax
αIsc
βVoc
αIpmax
βVpmax
IPmax low
VPmax low
IPmax NOCT
VPmax NOCT
NREL Autosolver Date
NREL Autosolver Time
a_ref
I_L_ref
I_o_ref
R_s_ref
R_sh_ref
Adj
Mounting
N_p
Type
Short Side
Long Side
Geometric Multiplier
P2/Pref
CEC Listing Date
Last Update