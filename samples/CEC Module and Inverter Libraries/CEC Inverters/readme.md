# CEC Inverter Database Notes

These are instructions for downloading and processing inverter data from the California Energy Commission (CEC), and notes for keeping track of issues with the database.

The "CEC database" is data provided by the CEC.

The "SAM library" is the text file the SAM user interface reads to populate the the inverter list on the Detailed Photovoltaic model's Inverter Database UI form.

#### Overall Steps

1. Download CEC Excel files from CEC website.

2. Manually clean up data in file and save as CSV.

3. Run `generate_inverter_library_file.lk` to convert data to SAM library file CSV.

4. Open `inverter_test.sam` and run `inverter_test_simulations.lk` to test data in library file and remove inverters with bad data.

5. Run `test_script_ow.lk` in case default inverter parameters change the SAM results.

## 1. Download files from CEC

1. Create a new working folder for this version of the inverter library in `Developer Tools and Instructions/Module and Inverter Database Tools/CEC Inverters`, like `CEC Inverters yyyy-m-d` with today's date in the folder name.

	As of January 2019, we will not include legacy data in the SAM library.

2. Go to the Grid Support Solar/Battery Inverters page at https://solarequipment.energy.ca.gov/Home/InvertersList and click **Download Excel File** to download the "Full Data" version of inverter list and save them to the folder with the CEC update date appended to the file name, for example `Grid_Support_Inverter_List_Full_Data_ADA_2023-11-17.xlsm`:

## 2. Process files

This manual process could be done automatically, but the manual conversion is useful to do a visual check of the data. 

1. Open the workbook in Excel.

2. Save each worksheet (solar inverters and battery inverters) as a tab-delimited .txt file. Use tab delimiter so we can remove commas from description and other fields later.

3. Close Excel and open the file in a text editor that can search and replace using extended characters and regular expressions like Notepad++.

4. Delete rows up to the header row that starts with "Manufacturer".

5. Some cells have soft line breaks in Excel that need to be removed, assuming Windows with CRLF line endings: 

	a. Replace LF `\n` with space ` `.
	
	b. Replace CR `\r` with CRLF `\r\n`.
	
	c. Replace LF space `\n ` with LF `\n`.

6. Replace comma `,` with hyphen space `- `.

7. Replace double quotes `"` with empty string.

8. Replace double space `  ` with single space ` ` two or three times until there are no more double spaces.

9. Replace `\t\t` with `\tempty\t` a couple of times until there are no more occurrences to ensure empty cells are recognizable.

10. Replace `\t\r` with `\tempty\r` for empty cells at end of rows.

11. Type `empty` in the first column of the second row.

12. Replace `\t` with `,` to make file comma-delimited.

13. Use regular expression option to replace `[^\x00-\x7F]+` with empty string. (There should be at least 3 occurrences.) This removes non-ASCII characters that appear as garbage in the library as described here: https://github.com/NREL/SAM/issues/160.

14. Save as .csv and open in Excel for a visual check. Do not save in Excel to avoid reformatting dates and other numbers.

15. Repeat for solar and battery inverters workbook.

## 3. Run scripts to convert data to SAM library file

1. Start SAM, and run the `read_cec_inverter_data.lk` script to combine data from the solar inverter and battery inverter CSV files into one file `inverter_data.csv`. The script is likely to fail as it finds problems with data in the files. When it does, use Excel and a text editor to look for problems like extra line breaks or columns in the two input files and fix them in the text editor (avoid saving the file in Excel).

2. Run the `generate_inverter_library_file.lk` script on `inverter_data.csv` to convert the file to a SAM CEC inverter library file. This should go quickly, if the script gets stuck, stop the script, open the `inverter_data.csv` file to see if there are any hidden characters in the file (use the "show all characters" option in Notepad++ to see them.)

3. Review the CEC Inverter Conversion Log files to identify any potential problems with inverters added from the new database.

## 4. Run library test script

1. Close all running instances of SAM.

2. Copy the new version of the CSV library file and replace the `CEC Inverters.csv` file in the deploy/libraries folder of the SAM Git branch you will be using to build the SAM release with a copy of the new SAM library file with the new CSV library file.

3. Use that version of SAM to open the `inverter_test.sam` file, run the `inverter_test_simulations.lk` test script, and analyze the resulting efficiency data to identify any additional problems. In general, inverters with peak efficiencies of around 90% or less tend to have data problems.

## 5. Run defaults test script

1. Run defaults test `script test_script_ow.lk` to verify that default PV configurations work and in case default inverter parameters have changed enough to affect outputs.

    If you are building module and inverter libraries, run the defaults test script with new libraries for both modules and inverters.

## Notes and Issues

Make sure Excel does not change mm/dd/yyyy date to a number.

Very small numbers should be in scientific notation so they don't get interpreted as zeroes.

### Voltage Ratings

The CEC database provide minimum and maximum voltage ratings that we assume are the MPPT minimum and maximum ratings. The database does not provide a separate maximum voltage rating, Vdcmax provided on some inverter datasheets. For example the SAM Sunny Boy inverters [here](https://files.sma.de/dl/27676/SB30-77-US-DUS184327W.pdf) have maximum MPPT voltage ratings of 480 and maximum DC voltage of 600.

The `pvsamv1` compute module issues a simulation warning for timesteps where the DC voltage at the inverter MPPT input (`inverterMPPT1_DCVoltage`) exceeds the inverter Vdcmax rating -- see `inverter_vdcmax_check()` function. This warning is a vestige from before SAM checked modeled inverter MPPT ratings, e.g., see SAM 2019.7.31. Because SAM now limits the inverter voltage to the MPPT ratings, the voltage at an inverter input should never reach Vdcmax (unless it is lower than MPPT-hi, which should never be the case).

This [paper from SMA explains](https://files.sma.de/dl/7680/PV-Ausl-TI-en-10.pdf) how SMA inverters respond to input voltages outside of the inverter MPPT range.

* Suggest making Vdcmax an editable input, and populating it with MPPT-hi from library. Add UI text to tell user to change it to datasheet value for sizing purposes, and for system sizing macro.

