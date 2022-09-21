This folder contains scripts to automatically update the files in deploy/runtime/defaults with new values from the latest ATB

To run:
1. Ensure that the download path in atb_defaults_update.py reflects the latest ATB (ATB is typically released in June or July)
2. Install in a Python enviroment with pandas
3. Run python atb_defaults_update.py

Due to available data, the following still need to be updated manually:

1. Detailed PV and PV Watts O&M costs - the machine readable ATB data does provide a detailed breakdown, and using these values would double count inverter replacements
2. Commercial and Residential PV+Battery systems: the ATB does not display PV+Battery at this scale, so using the costs here would not include cost benifits of co-location
3. Residential and commercial wind: distributed wind does not provide a cost breakdown