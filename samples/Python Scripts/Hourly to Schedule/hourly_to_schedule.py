import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import csv
plt.rcParams['font.size'] = 7.5
plt.rcParams['font.sans-serif'] = 'Arial'
plt.rcParams['axes.linewidth'] = 0.5
plt.rcParams['xtick.major.width'] = 0.5
plt.rcParams['ytick.major.width'] = 0.5
   

if True:
    data = np.genfromtxt('lcgs_sam_8760.csv', delimiter = ',')
    data_name = 'lcgs'
    first_day = 3 # Day of week on first day of the year (0 = Sunday, 6 = Saturday).  Used to determine weekday/weekend averages
else:
    data = np.genfromtxt('caiso_2019_sam.csv', delimiter = ',')
    data_name = 'caiso19'
    first_day = 2 # Day of week on first day of the year (0 = Sunday, 6 = Saturday).  Used to determine weekday/weekend averages
          
n_avg_hours = 2             # Integer number of hours over which data will be averaged 
n_avg_months = 3            # Integer number of months over which data will be averaged

nbins = 7                          # Number of bins to use for TOD multipliers
binning_strategy = 'equal_range'   # 'equal_number': equal number of time periods per bin, 'equal_range': equal range for each bin
tod_avg_to_one = True              # Force TOD schedule to average to 1.0?

cap_outliers = False   # Cap outlying values before averaging?
cap_stdev = 3.0        # Maximum/Minimum allowable values (number of standard deviations)

sname = data_name + "_" + str(nbins) + "bins_" + str(n_avg_hours) + "hrs_" + str(n_avg_months) + "months_" + binning_strategy

#------------------------------------------------------------------------------
def remove_outliers(data, stdev):
    max_allowable = data.mean() + cap_stdev*data.std()
    min_allowable = data.mean() - cap_stdev*data.std()
    
    inds = np.where(data>max_allowable)[0]
    data[inds] = max_allowable
    if len(inds)>0:
        print('Capping %d high outliers to %.3f'%(len(inds), max_allowable))
    
    inds = np.where(data<min_allowable)[0]
    data[inds] = min_allowable
    if len(inds)>0:
        print('Capping %d low outliers to %.3f'%(len(inds), min_allowable))  
    return data



# data = annual data array, first_day = index for day of week on first day (0 = Sunday, 6 = Saturday)
def calculate_averages(data, first_day):
    n = len(data)
    nperhour = int(n/8760)
    nperday = nperhour*24
    tstep = 1./(nperday/24)

    if cap_outliers:
        data = remove_outliers(data, cap_stdev)   
        
    dow = np.append( np.repeat(np.arange(first_day, 7), nperday), np.tile(np.repeat(np.arange(7), nperday), 53))[0:n]  # Day of week
    is_weekday = np.ones(n, dtype = bool)
    is_weekday[dow == 0] = False
    is_weekday[dow == 6] = False
    
    days_per_month = np.array([31,28,31,30,31,30,31,31,30,31,30,31], dtype = int)
    days_per_avg_month = np.reshape(days_per_month, (-1, n_avg_months)).sum(1) 
    
    df = {'hour': np.tile(np.arange(0,24,tstep).astype(int),365),
          'hour_avg': np.tile((np.arange(0,24,tstep)/n_avg_hours).astype(int),365),
          'day': np.repeat(np.arange(0,365,1), nperday),
          'month':np.repeat(np.arange(12), nperday*days_per_month),
          'month_avg': np.repeat(np.arange(12/n_avg_months), nperday*days_per_avg_month),
          'day of week': dow,
          'is_weekday': is_weekday,
          'data': data
          }
    df = pd.DataFrame(df)

    n_weekday = len((df.loc[df['is_weekday'] == True]))  # Total number of weekday points
    n_weekend = len((df.loc[df['is_weekday'] == False]))  # Total number of weekend points

    # Calculate averages
    weekday = (df.loc[df['is_weekday'] == True]).pivot_table(values = 'data', index = 'month_avg', columns = 'hour_avg', aggfunc = 'mean').values     # 2D array with weekday data: rows = month, columns = hour
    weekend = (df.loc[df['is_weekday'] == False]).pivot_table(values = 'data', index = 'month_avg', columns = 'hour_avg', aggfunc = 'mean').values    
     
    # Expand back to full 12 month by 24 hour shape
    weekday = np.repeat(np.repeat(weekday, n_avg_months, axis = 0), n_avg_hours, axis = 1)
    weekend = np.repeat(np.repeat(weekend, n_avg_months, axis = 0), n_avg_hours, axis = 1)
    
    return weekday, weekend, n_weekday, n_weekend


def calculate_bins(weekday, weekend, n_weekday, n_weekend, binning_strategy, nbins = 9, avg_to_one = True):
    weekday_bins = np.zeros((12, 24), dtype = int)  # Bin ID for each weekday point
    weekend_bins = np.zeros((12, 24), dtype = int)  # Bn ID for each weekend point
    averages = np.zeros(nbins)                      # Average value in each bin
    bounds = np.zeros((nbins, 2))                   # Min/max bounds of each bin    
    
    vals = np.append(weekday.flatten(), weekend.flatten())  # Columns followed by rows (e.g. 0:24 is weekday[0,:])
    inds = vals.argsort() 
    n = len(vals)

    # Set bins such that each one contains an equal number of points, regardless of bin size 
    if binning_strategy == 'equal_number':
        nperbin = np.array([int(n/nbins) for j in range(nbins)])
        nperbin[-1] += (n - nbins*nperbin[0])  # Assign extra points to last bin
        for j in range(nbins):
            i = 0 if j == 0 else nperbin.cumsum()[j-1]
            data = vals[inds[i:i+nperbin[j]]]
            bounds[j,0] = data.min()
            bounds[j,1] = data.max()     
            
    # Set bins with even ranges between the min/max points.  Note this does not guarantee that all bins will actually contain points      
    elif binning_strategy == 'equal_range':
        interval = (vals.max()-vals.min()) / nbins
        bounds[:,0] = np.linspace(vals.min(), vals.max()-interval, nbins)
        bounds[:,1] = bounds[:,0] + interval
        
    # TODO: Set bins bounds to minimize deviation between actual time-period averages and bin averages?
        
        
    # Assign each point to a bin and calculate averages
    for j in range(nbins):
        weekday_bins[np.where(weekday>=bounds[j,0])] = j
        weekend_bins[np.where(weekend>=bounds[j,0])] = j   
    
    bins = np.append(weekday_bins.flatten(), weekend_bins.flatten())
    nperbin = np.unique(bins, return_counts = True)[1]
    averages = np.array([vals[bins==j].mean() for j in range(nbins)])
        
    # Adjust bin-average so that mulitpliers average to 1 over the year
    if tod_avg_to_one:
        avgval = (n_weekday*averages[weekday_bins].mean() + n_weekend*averages[weekend_bins].mean())/(n_weekday + n_weekend)
        averages *= (1.0/avgval)

    return averages, weekday_bins, weekend_bins, nperbin
    




# Plot hourly/monthly averages, and results after categorizing into bins
weekday, weekend, n_weekday, n_weekend = calculate_averages(data, first_day)
averages, weekday_bins, weekend_bins, nperbin = calculate_bins(weekday, weekend, n_weekday, n_weekend, binning_strategy, nbins, tod_avg_to_one)

lims = [min(weekday.min(), weekend.min()), max(weekday.max(), weekend.max())]
fig, ax = plt.subplots(2, 2, figsize = (6.5, 4))  
for j in range(2):
    for i in range(2):
        plt.sca(ax[i,j])
        if i == 0:
            plotdata = weekday if j == 0 else weekend
        else:
            plotdata = averages[weekday_bins] if j == 0 else averages[weekend_bins]  
        plt.imshow(plotdata, cmap = 'magma', vmin = lims[0], vmax = lims[1], aspect = 'auto')
        plt.colorbar(shrink = 1.0, aspect = 15)
        plt.xlabel('Hour')
        plt.ylabel('Month')
        if i == 0:
            plt.annotate('Weekday' if j == 0 else 'Weekend', xycoords = 'axes fraction', xy = [0.5, 1.05], va = 'bottom', ha = 'center', annotation_clip = False, fontsize = 8)
fig.tight_layout()

plt.savefig(sname + "_tod_PRICES.png")


fig, ax = plt.subplots(2, 1, figsize = (7.5, 4))  
for j in range(2):
    plt.sca(ax[j])

    plotdata = weekday_bins if j == 0 else weekend_bins

    plt.imshow(plotdata, cmap = 'magma', aspect = 'auto', interpolation='none')
    plt.colorbar(shrink = 1.0, aspect = 15)
    plt.xlabel('Hour')
    plt.ylabel('Month')

    plt.annotate('Weekday' if j == 0 else 'Weekend', xycoords = 'axes fraction', xy = [0.5, 1.05], va = 'bottom', ha = 'center', annotation_clip = False, fontsize = 8)
fig.tight_layout()

plt.savefig(sname + "_tod_BINS.png")

fig, ax = plt.subplots(2, 1, figsize = (6.5, 4))  
for j in range(2):

    plt.sca(ax[j])

    plotdata = averages[weekday_bins] if j == 0 else averages[weekend_bins]  
        
    nmonth = int(12/n_avg_months)
    for k in range(nmonth):
        m = n_avg_months*k
        plt.plot(np.arange(24), plotdata[m,:], lw = 1.25, label = '%d - %d'%(m, m+n_avg_months-1))    
        
    plt.xlim(0,23)
    plt.xlabel('Hour')
    plt.ylabel('Multiplier')
    plt.legend()

    plt.annotate('Weekday' if j == 0 else 'Weekend', xycoords = 'axes fraction', xy = [0.5, 1.05], va = 'bottom', ha = 'center', annotation_clip = False, fontsize = 8)

fig.tight_layout()

plt.savefig(sname + "_Duck_Curves.png")


with open(sname + "_weekday_schedule.csv", "w", newline='') as csv_file:
    writer = csv.writer(csv_file, delimiter=',')
    for line in weekday_bins:
        line_out = [x+1 for x in line]
        writer.writerow(line_out)

with open(sname + "_weekend_schedule.csv", "w", newline='') as csv_file:
    writer = csv.writer(csv_file, delimiter=',')
    for line in weekend_bins:
        line_out = [x+1 for x in line]
        writer.writerow(line_out)

with open(sname + "_tod_period_prices.csv", "w", newline='') as csv_file:
    writer = csv.writer(csv_file, delimiter=',')
    for i, price in enumerate(averages):
        writer.writerow([i+1, price])