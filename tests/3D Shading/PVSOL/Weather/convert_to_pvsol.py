import pandas as pd
import os

files = os.listdir(os.getcwd())

# for now assume relative humidity isn't important
RH = 0

for f in files:
    if '.csv' in f:
        print str(f)
        # extract location
        ind = 0
        for c in f:
            if c == ' ':
                location = f[0:ind]
                break
            ind += 1


        # read csv data
        df_head = pd.read_csv(f, nrows=1)
        df = pd.read_csv(f, skiprows=2)

        # create columns if don't exist due to misformating
        is_iwec = False
        if not 'Temperature' in df:
            df['Temperature'] = df['Tdry']
            df['Wind Speed'] = df['Wspd']
            is_iwec = True

        # Meteonorm format uses negative longitude east, contrary to everyone else.
        df_head['Longitude'][0] *= -1

        if is_iwec:
            df_head['Elevation'][0] = 0

        # write dat file in PVSOL format
        file_pvsol = str(location) + "_pvsol.dat"
        print "Writing: " + str(file_pvsol)
        fout = open(file_pvsol, 'w')
        fout.write(location + '_converted\n')
        fout.write(str(df_head['Latitude'][0]) + ',' +
                   str(df_head['Longitude'][0]) + ',' +
                   str(df_head['Elevation'][0]) + ',' +
                   str(-df_head['Time Zone'][0]) + ',' +
                   str(-30) + '\n\n')
        fout.write('Ta\tGh\tFF\tRH\t\n')
        for i in range(0, len(df)):
            fout.write(str(round(df['Temperature'][i], 1)) + "\t" +
                       str(round(df['GHI'][i], 0)) + "\t" +
                       str(round(df['Wind Speed'][i], 1)) + "\t" +
                       str(RH) + "\t\n")

        fout.close()
