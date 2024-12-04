from ast import Try
import pandas as pd
import urllib.request
import urllib.error
import re
import os


def get_load_data(year, dataset, upgrade, bldg_id, state, path):
    """ Get load data for a given building ID. Save three files:
            {bldg_id}.parquet
            load-data-all-{bldg_id}.csv
            load-data-total-{bldg_id}.csv

        Available years as of December 2022:
            2021
            2022

        Available datasets:
            comstock_amy2018_release_1/		
            comstock_tmy3_release_1/		
            resstock_amy2018_release_1/		
            resstock_tmy3_release_1/
        
        Available upgrades depends on dataset. See upgrade_dictionary.tsv file in dataset
        folder for a list.

        Available building ids depends on dataset and are listed in metadata.parquet file 
        for each dataset.

        Args:
            year (string): Dataset year, e.g. '2022'
            dataset (string): Name of dataset, e.g., 'comstock_amy2018_release_1/'
            upgrade (string): Upgrade number, e.g., 10
            bldg_id (string): Building ID, e.g., '86902'
            path (string): path to working folder to read and write files

        Returns:
            fname (string): Path to load data parquet file
    """
    
    # skip download if file already exists
    fname_parquet = f'{path}/{bldg_id}.parquet'
    if os.path.exists(fname_parquet):
        print(f'Skipping load file download. File already exists:\n{fname_parquet}\n')

    # metadata folder uses 'baseline', by_state folder uses '0'
    if upgrade == 'baseline':
        upgrade = 0

    url = f'https://oedi-data-lake.s3.amazonaws.com/' \
    f'nrel-pds-building-stock/' \
    f'end-use-load-profiles-for-us-building-stock/' \
    f'{year}/' \
    f'{dataset}/' \
    f'timeseries_individual_buildings/' \
    f'by_state/' \
    f'upgrade={upgrade}/' \
    f'state={state}/' \
    f'{bldg_id}-{upgrade}.parquet'

    print(f'Getting load data:\n'
          f'    {year}\n'
          f'    {dataset}\n'
          f'    upgrade {upgrade}\n'
          f'    {state}\n'
          f'    {bldg_id}\n'
          f'from {url}\n')

    try:
        parquet_file = urllib.request.urlopen(url)
    except urllib.error.HTTPError as err:
        print(f'HTTP Error {err.code}: Error 404 may indicate bldg_id {bldg_id} is not in this dataset.')

    with open(fname_parquet, 'b+w') as f:
        f.write(parquet_file.read())

    # read data into a dataframe
    df = pd.read_parquet(fname_parquet)
    df = df.set_index('timestamp')
    df = df.shift(periods=-1, freq='infer') # timestamps start at 0:15 so shift to start at 0:00

    # convert entire file to csv to have a record of data
    df.to_csv(f'{path}/load-data-all-{bldg_id}.csv', index=True)

    # convert only time stamp and total electricity columns to csv to manually paste into SAM
    df.to_csv(f'{path}/load-data-total-{bldg_id}.csv', columns=['out.electricity.total.energy_consumption'], index=True)

    return fname_parquet


def get_weather_data(state, county_code, year, dataset, path):
    """ Given state abbreviation, county code, and weather file type, download weather file from NREL Building Stock.

        Notes:
        * Weather files do not include lat, lon, tz, elev header data required by SAM.
        * Weather files are hourly, load data is 15-minute.

        Weather file types (wf_type) available as of December 2022:
            amy2012 (resstock)
            amy2018 (resstock and comstock)
            tmy3 (resstock and comstock)

        Args:
            state (string): State abbreviation, e.g., 'AZ' for Arizona.
            county_code (string): County gisjoin code. Use get_county_code() to retrieve code.
            wf_type (string): Weather file type, e.g., amy2018 for annual meteorological year 2018.

        Returns:
            wfname (string): Path for downloaded weather file.
    """

    # get string for year designation in file name
    wf_type = dataset.split('_')[1] # assumes dataset name like 'comstoc_tmy3_release_1'
    if wf_type == 'tmy3':
        wf_year = 'tmy3'
    else:
        wf_year = re.findall('\d+',wf_type)[0] # e.g., extract 2018 from amy2018

    # skip download if weather file already exists
    wfname = f'{path}/weather-data-{year}-{dataset}-{county_code}-{wf_year}.csv'
    if os.path.exists(wfname):
        print(f'Skipping weather file download. File already exists:\n{wfname}\n')
        return wfname


    # 2021 stores eather files in folders by wf_type
    # 2022 stores weather files in folders by state
    if year == '2021':
        wf_folder = f'{wf_type}'
    else:
        wf_folder = f'state={state}'


    url = f'https://oedi-data-lake.s3.amazonaws.com/' \
    f'nrel-pds-building-stock/' \
    f'end-use-load-profiles-for-us-building-stock/' \
    f'{year}/' \
    f'{dataset}/' \
    f'weather/' \
    f'{wf_folder}/' \
    f'{county_code}_{wf_year}.csv'

    print(f'Getting weather data:\n'
          f'    {year}\n'
          f'    {dataset}\n'
          f'    {county_code}\n'
          f'    {wf_year}\n'
          f'from {url}\n')

    wf = urllib.request.urlopen(url)

    with open(wfname, 'b+w') as f:
        f.write(wf.read())

    return wfname

def get_county_code(state, county, year, dataset, path):
    """ Given a state abbreviation and county name, get a lookup table file from Resstock or Comstock
        and return a county code (nhgis_2010_county_gisjoin) from table.

        Lookup table is 'spatial_tract_lookup_table.csv' in 'geographic_information' folder for each available 
        dataset at https://data.openei.org/s3_viewer?bucket=oedi-data-lake&prefix=nrel-pds-building-stock%2Fend-use-load-profiles-for-us-building-stock

        As of December 2022, available years are:
            2021
            2022

        And available datasets are:
            comstock_amy2018_release_1/		
            comstock_tmy3_release_1/		
            resstock_amy2018_release_1/		
            resstock_tmy3_release_1/

        Args:
            state (string): Two letter state abbreviation, case insensitive, e.g., 'AZ' or 'az' for Arizona.
            county (string): County name, case insensitive, e.g., 'maricopa', 'Maricopa', or 'Maricopa County'.
            year (string): Dataset year.
            dataset (string): Dataset name.

        Returns:
            County code as string, e.g., 'G0400130' for Maricopa County, Arizona.
    """

    url_path = dataset
    if year == '2024' and "resstock" in dataset:
        url_path = "resstock_dataset_2024.1/resstock_tmy3"

    # url to lookup table for year and dataset
    url = f'https://oedi-data-lake.s3.amazonaws.com/' \
    f'nrel-pds-building-stock/' \
    f'end-use-load-profiles-for-us-building-stock/' \
    f'{year}/' \
    f'{url_path}/' \
    f'geographic_information/' \
    f'spatial_tract_lookup_table.csv'

    print(f'Getting county code:\n'
          f'    {county}\n'
          f'    {state}\n'
          f'from {url}\n')

    # download lookup table file, convert to pandas dataframe, and remove unneeded data
    csv_file = urllib.request.urlopen(url)

    fname = f'{path}/lookup-table-{year}-{dataset}.csv'
    with open(fname, 'b+w') as f:
        f.write(csv_file.read())

    df = pd.read_csv(fname, dtype='unicode')
    
    # column names in lookup table are different for different years
    if year == '2021':
        co = 'resstock_county_id'
        st = 'state_abbreviation'
        gj = 'nhgis_county_gisjoin'
    else:
        co = 'county_name'
        st = 'state_abbreviation'
        gj = 'nhgis_2010_county_gisjoin'

    df.filter(items=[co, st, gj])
    df = df.drop_duplicates(subset=[co])
    x = df.loc[df[st].str.contains(state, case=False) & 
               df[co].str.contains(county, case=False), gj]

    # return gisjoin for state and county name    
    return x.values[0]

def get_metadata(county_code, year, dataset, upgrade, path):
    """ Get metadata for given year, dataset and county.
        Saves parquet file with metadata for entire dataset.
        Saves csv file with metadata for one county.

        Args:
            county_code (string): gisjoin code for county from dataset.
            year (string): Dataset year.
            dataset (string): Dataset name.
            upgrade (string): Upgrade number.
            path (string): Path to store files.

        Returns:
            list of fname_csv (string): Path to csv file with metadata for county 
                    fname_parquet (string): Path to parquet file with metadata for entire dataset
    """

    fname_parquet = f'{path}/metadata-{year}-{dataset}-upgrade{upgrade}.parquet'
    if os.path.exists(fname_parquet):
        print(f'Skipping metadata file download. File already exists:\n{fname_parquet}\n')
    else:
        # 2021 has a single file named metadata.parquet
        # 2022 data has separate file for each upgrade with names like upgrade02.parquet
        if year == '2021':
            str_upgrade =  'metadata'
        else:
            if any(char.isdigit() for char in upgrade):
                if int(upgrade) < 10:
                    str_upgrade = f'upgrade0{upgrade}'
                else:
                    str_upgrade = f'upgrade{upgrade}'
            else:
                str_upgrade = upgrade # upgrade may be 'baseline'

        url = f'https://oedi-data-lake.s3.amazonaws.com/' \
        f'nrel-pds-building-stock/' \
        f'end-use-load-profiles-for-us-building-stock/' \
        f'{year}/' \
        f'{dataset}/' \
        f'metadata/' \
        f'{str_upgrade}.parquet'
    
        print(f'Downloading metadata:\n'
              f'   {year}\n'
              f'   {dataset}\n'
              f'   upgrade {upgrade}\n'
              f'from {url}\n')

        parquet = urllib.request.urlopen(url)

        with open(fname_parquet, 'b+w') as f:
            f.write(parquet.read())


    fname_csv = f'{path}/metadata-{county_code}-{year}-{dataset}-upgrade{upgrade}.csv'
    if os.path.exists(fname_csv):
        print(f'Skipping reading county data from meta data file. County metadata csv file already exists\n{fname_csv}\n')
    else:
        print('Reading metadata from file and writing county data to csv.\n')
        df = pd.read_parquet(fname_parquet)
        df = df.loc[df['in.county'] == county_code]
    
        with open(fname_csv, 'w') as f:
            df.to_csv(fname_csv)

    meta = [fname_csv, fname_parquet ]

    return meta


def main():
    year = '2022'
    dataset = 'resstock_amy2018_release_1'
    upgrade = 'baseline'
    state = 'CO'
    county_name = 'Jefferson'


    path = ''

    if path == '':
        print('Set \'path\' to folder for storing files.')
        quit()

    if os.path.exists(path) == False:
        os.mkdir(path)

    county_code = get_county_code(state, county_name, year, dataset, path)

    meta = get_metadata(county_code, year, dataset, upgrade, path)

    bldg_id = ''

    if bldg_id == '':
        print(f'Get building id from {meta[0]}, set value of bldgs, and rerun script.')
        quit()

    get_weather_data(state, county_code, year, dataset, path)

    get_load_data(year, dataset, upgrade, bldg_id, state, path)

    print(f'Done. See {path} for downloaded files.')
    
if __name__ == "__main__":
    main()