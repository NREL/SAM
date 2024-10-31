import os
import pandas as pd

from eulp import get, plot, stats

###############################################################################################################
# 
#  This script demonstrates how to use the eulp module to access individual building load data in the 
#  OpenEI Open Energy Initiative (OEDI) data lake "End-Use Load Profiles for the U.S. Building Stock."
#
#  These instructions assume you are downloading load data for a U.S. county from either the Resstock 
#  (residential buildings) or Comstock (commercial buildings) dataset. The datasets #  contain files for 
#  thousands of buildings for each county. The loadster module does not provide #  tools to help you choose 
#  buildings from the database.
#  
#  Before using this script, go to https://data.openei.org/submissions/4520, follow the "View Data  Lake" 
#  link for "Aggregate and Individual Building Timeseries End Use Load Profiles," and decide what values you
#  want to use for the following parameters:
#  
#  * year: The year the data was prepared. As of December 2022, available years are 2021 and 2022.
#
#    Find available years in: ../end-use-load-profiles-for-us-building-stock/.
#
#    For example, for 2022, set year = '2022'
#  
#  * dataset: For a short description of Resstock and Comstock, see the FAQ at
#    https://comstock.nrel.gov/page/faq#what-buildings-are-represented-by-each-dataset.
#    
#    Find available datasets in: ../end-use-load-profiles-for-us-building-stock/[year].
#
#    For example, for ../2021/comstock_amy2018_release_1/, set dataset = 'comstock_amy2018_release_1'.
#
#  * upgrade: Upgrade is the level of energy efficient upgrades for the building, ranging from baseline to
#    level 1-10 for the 2022 datasets, and a single level 0 for 2021. The tab-delimited text file 
#    upgrade_dictionary.tsv is in the folder for each dataset that describes the upgrades.
#    
#    Find the upgrade number(s) in:
#    ../end-use-load-profiles-for-us-building-stock/[dataset]/timeseries_individual_buildings/by_state/.
#
#   (Data is also available in the by_county folder, but this script assumes the by_state folder.)
#
#    For example, for "upgrade=0/", set upgrade = '0'.
#
#  To use this script:
#
#    1. Set the parameters you found above in Steps 1-4 below and run the script.
#
#       The script downloads metadata for all of the data in the dataset and
#       creates a CSV file listing available buildings for one county.
#       
#    2. Manually choose one or more building IDs from the CSV file and set
#       the value of buldings in Step 5 below.
#
#    3. Run the script again.
#    
#      The script generates CSV files of load and weather data, generates
#      plots of the load data, and calculates peak and total statistics.
#
###############################################################################################################

## Set these parameters before running the script! ##

# 1. Choose a U.S. state and county:
state = 'CO'
county_name = 'Jefferson'

# 2. Get these parameters from the data lake website:
year = '2022'
dataset = 'resstock_amy2018_release_1'
upgrade = 'baseline'

# 3. Set the path where you want to store files
# (files are large, you'll need at least 200 MB of space):
path=''

## End Parameters ##

# create folder if it doesn't exist
if os.path.exists(path) == False:
    os.mkdir(path)

# county_code is a gisjoin identifier from the database that we need to identify the county
county_code = get.get_county_code(state, county_name, year, dataset, path)

# get metadata
meta = get.get_metadata(county_code, year, dataset, upgrade, path)

## This manual step is required to choose a building ##

# 4. Look at the metadata CSV file and choose a building from the bldg_id column:
bldg_id = ''

## End Manual Step ##

# get load data
if bldg_id == '':
    print(f'Set the value of \'bldg_id\' to the bldg_id value from the metadata csv file and then run the script again:\n{meta[0]}\n\nQuitting script.' )
    quit()

get.get_load_data(year, dataset, upgrade, bldg_id, state, path)


# get weather data
# weather data used as input to the building model to generate the load data
# may be useful for reference. (Does not include lat, lon, elev, tz data required by SAM)
get.get_weather_data(state, county_code, year, dataset, path)

# plot load data
fname = f'{path}/load-data-all-{bldg_id}.csv'

df = pd.read_csv(fname, 
                 parse_dates=True, 
                 index_col='timestamp')

D = plot.clean_plot_data(df, bldg_id) 
df = D['df_clean']

plot.plot_full_year(df.filter(items=['total']), bldg_id, 'Total Load') # total load over year
plot.plot_segment(df, bldg_id, False, '06', '21') # appliance and load around summer solstice
plot.plot_segment(df, bldg_id, False, '12', '21') # appliance and total load around winter solstice

df_total = df.filter(items=['total']) # total only for some plots

# Show annual and monthly statistics for total load
fname = f'{path}/load-data-total-{bldg_id}.csv'

df = pd.read_csv(fname,
                parse_dates=True,
                index_col='timestamp')

print(f'== Building {bldg_id} ==\n')
print('All Stats')
data = stats.calculate_stats(df)
print(data)
print()

print('Annual Stats')
for stat in data['annual']:
    print(f"{stat} = {data['annual'][stat]}")
print()

print('Monthly Stats')
for stat in data['monthly']:
    print(f"{stat} = {data['monthly'][stat]}")
