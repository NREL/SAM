import pandas as pd

def calculate_stats(df):
    """ Given a load file from OpenEI, return total and peak load by year
        and month.
    
    Args:
        df (Pandas dataframe): Time series load data as imported from OpenEI by loadster.get module.
        
    Returns:
        Dictionary of:
            peak_annual (dictionary of {timestamp, float}): Maximum load value over year and time peak occurred.
            peak_monthly (dictionary of {timestamp, float}: List of maximum load load value and time for each month.
            total_annual (float): Sum of values over year.
            total_monthly (list of floats): List of monthly totals.
    """
 
    # prepare dataframe
    df = df.reset_index()
    df = df.set_index('timestamp')
    df = df.rename(columns={'out.electricity.total.energy_consumption': 'total_load'})

    # calculate statistics
    ts_per_hour = len(df.index) / 8760

    annual_total = sum(df['total_load'] / ts_per_hour)
    annual_peak = max(df['total_load'])
    annual_peak_time = str(df.idxmax()[0])

    monthly_total = (df.groupby(df.index.month)['total_load'].sum() / ts_per_hour).to_list()
    monthly_peaks = (df.groupby(df.index.month)['total_load'].max()).array.tolist()
    monthly_peak_times = (df.groupby(df.index.month)['total_load'].idxmax()).array.tolist()
    monthly_peak_times = [str(time) for time in monthly_peak_times]

    return {  'annual': { 'total_kwh': annual_total, 
                          'peak_kw':  annual_peak,
                          'peak_time':  annual_peak_time}, 
              'monthly': { 'total_kwh': monthly_total,
                           'peak_kw': monthly_peaks,  
                           'peak_time': monthly_peak_times } }


def main():
    
    # set parameters
    bldg_id = ''
    path = ''

    if bldg_id == '' or path == '':
        print('Set \'path\' to folder containing {bldg_id}.parquet file and \'bldg_id\' to bldg_id value.')
        quit()

    # read load data, calculate and display load statistics
    fname = f'{path}/{bldg_id}.parquet'

    df = pd.read_parquet(fname, columns=['timestamp', 'out.electricity.total.energy_consumption'])
    
    stats = calculate_stats(df)
        
    print('All Stats')
    print(stats)
    print()

    print('Annual Stats')
    for stat in stats['annual']:
        print('{} = {}'.format(stat,stats['annual'][stat]))
    print()

    print('Monthly Stats')
    for stat in stats['monthly']:
        print('{} = {}'.format(stat, stats['monthly'][stat]))


if __name__ == "__main__":
    main()