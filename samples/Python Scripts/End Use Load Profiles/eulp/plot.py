import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as ticker
import seaborn as sns
import colorcet as cc
from datetime import timedelta

def clean_plot_data(df, bldg_id):
    """ Remove unneeded columns from OpenEI load data and sort columns
        by total annual values.
        
    
    Args:
        df (Pandas dataframe): Time series load data from OpenEI.
        bldg_id (string): OpenEI building id number to use in plot titles.
        
    Returns:
        Dictionary of:
            df_clean (Pandas dataframe): Time series data of non-zero 
                electricity load sorted by descending total annual value.
    """
 
    # remove unneeded columns
    df = df.filter(regex='out.electricity') # keep only electricity outputs
    unneeded_columns = ['intensity', 'out.electricity.pv', 'out.electricity.net.energy_consumption']
    for x in unneeded_columns:
        df = df[df.columns.drop(list(df.filter(regex=x)))]
 
    # remove columns with zero usage and collect column totals
    empty_columns = []
    column_totals = {}
    for name, values in df.items():
        x = name.replace('out.electricity.','')
        x = x.replace('.energy_consumption','')
        df = df.rename(columns={name: x})
        if sum(values) == 0:
            df = df[df.columns.drop(x)]
            empty_columns.append(x)
        else:
            column_totals[x] = sum(values)
 
    # sort columns in order of decreasing total of kW values
    sorted_columns = dict(sorted(column_totals.items(), key=lambda item: item[1], reverse=True))

    df_sorted = pd.DataFrame()
    df_sorted.index.name = 'timeseries'
    df_sorted.index = df.index
    for key in sorted_columns:
        df_sorted.loc[:, key] = df[key]
       
    return{'bldg_id': bldg_id, 'df_clean': df_sorted}

def plot_full_year(df, bldg_id, str_title):
    """ Generates a plot window with a line graph of data over one year.
    
    
    Args: 
        df (Pandas dataframe): Time series load data with timestamps as index containing only data to plot.
        bldg_id (string): Building ID from OpenEI load data.
        str_title (string): String for plot title to insert in 'Building {bldg_id} {str_title}'.
        show (boolean): True to show image, False to save without showing.
    """
    # plot full year of data with months as x axis
    
    # set color palette for more than 12 colors
    n_colors = len(df.columns)
    sns.set_palette(cc.glasbey, n_colors)
    
    fig, ax = plt.subplots(figsize=(13.33,7.5))
    ax.plot(df.index, df)
    ax.xaxis.set_major_locator(mdates.MonthLocator())
    ax.xaxis.set_minor_locator(mdates.MonthLocator(bymonthday=16))
    ax.xaxis.set_major_formatter(ticker.NullFormatter())
    ax.xaxis.set_minor_formatter(mdates.DateFormatter('%b'))
    ax.tick_params(axis='x', which='minor', tick1On=False, tick2On=False)
    for label in ax.get_xticklabels(minor=True):
        label.set_horizontalalignment('center')
    imid = len(df.index) // 2
    ax.set_xlabel(str(df.index[imid].year))
    
    plt.legend(df.columns, loc='center left', bbox_to_anchor=(1,0.5))
    plt.tight_layout()
    plt.title(f'Building {bldg_id} {str_title}')
    plt.subplots_adjust(top=0.9)
    plt.margins(0, 0.2, tight=False)
    plt.show()

def plot_segment(df, bldg_id, stacked, mm, dd):
    """ Generates a plot window with a line or area graph for six days of data centerd at mm dd.
    
    
    Args:
        df (dataframe): Time series load data
        bldg_id (string): Building ID from OpenEI data
        stacked (boolean): True for area plot, False for line plot
        mm (string): Month as mm, e.g., '06' for June
        dd (string): Day of month dd, e.g, '21' for 21st
        show (boolean): True to show image, False to save without showing.
    """
    
    # store peak annual load for plot y-axis limit for line segment plot
    if not stacked:
        peak_load = max(df['total'])

    # get data segment
    year = df.index[0].year
    in_date = pd.to_datetime(str(year) + '-' + mm + '-' + dd)
    start_date = in_date - timedelta(days=3)
    end_date = in_date + timedelta(days=3)
    df = df.loc[start_date : end_date]
    
    # set color palette for more than 12 colors
    n_colors = len(df.columns)
    sns.set_palette(cc.glasbey, n_colors)

    column_totals = {}
    for name, values in df.items():
        column_totals[name] = sum(values)

    # sort columns in order of decreasing total of kW values over time segment
    sorted_columns = dict(sorted(column_totals.items(), key=lambda item: item[1], reverse=True))

    df_sorted = pd.DataFrame()
    df_sorted.index.name = 'timeseries'
    df_sorted.index = df.index
    for key in sorted_columns:
        df_sorted.loc[:, key] = df[key]
  
    # segment needs to be sorted because totals of segments are not in same order as totals of annual
    fig, ax = plt.subplots(figsize=(13.33,7.5))
    if stacked:
        y_values = {}
        for name, values in df_sorted.items():
            y_values[name] = values
        ax.stackplot(df_sorted.index, y_values.values(), labels=y_values.keys())
    else:
        plt.ylim(0,round(peak_load,1))
        plt.plot(df_sorted)

    plt.legend(df_sorted.columns, loc='center left', bbox_to_anchor=(1,0.5))
    ax.xaxis.set_major_locator(mdates.DayLocator())
    ax.xaxis.set_minor_locator(mdates.HourLocator(interval=6, tz=None))
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %d'))
    ax.xaxis.set_minor_formatter(mdates.DateFormatter('%H:%M'))
    ax.tick_params(axis="x", which="both", rotation=70)
    plt.subplots_adjust(right=0.7)
    plt.title(f'Building {bldg_id} {mm}-{dd}')
    plt.margins(0, 0.015, tight=True)

    plt.show()

def main():    
    bldg_id = ''
    path = ''

    if bldg_id == '' or path == '':
        print('Set \'path\' to folder containing load-data-all-{bldg_id}.csv file and \'bldg_id\' to bldg_id value.')
        quit()

    # read load data from OpenEI load file as imported by loadster.get
    fname = f'{path}/load-data-all-{bldg_id}.csv'
    df = pd.read_csv(fname, 
                     parse_dates=True, 
                     index_col='timestamp')

    # process data for plotting
    D = clean_plot_data(df) 
    df = D['df_clean']
    bldg_id = D['bldg_id']

    # choose data to plot
    df_total = df.filter(items=['total']) # total only for some plots
    df_no_total = df.drop(columns=['total']) # remove total for some plots

    ## sample plots ##

    # plot total load over year
    plot_full_year(df_total, bldg_id, 'Total Load')
    
    # plot all loads over year
    plot_full_year(df, bldg_id, 'All Loads with Total')

    # plot all loads over year without total
    plot_full_year(df_no_total, 'All Loads without Total', bldg_id)

    # area plots around summer solstice
    plot_segment(df_no_total, bldg_id, True, '06', '21')

    # line plots around summer solstice
    plot_segment(df, bldg_id, False, '06', '21')

    # area plots around winter solstice
    plot_segment(df_no_total, bldg_id, True, '12', '21')

    # line plots around winter solstice
    plot_segment(df, bldg_id, False, '12', '21')

if __name__ == "__main__":
    main()