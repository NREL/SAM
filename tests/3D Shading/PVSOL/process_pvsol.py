import pandas as pd
from matplotlib import pyplot as plt

class process_pvsol:

    df = pd.DataFrame()
    pv_efficiency = []
    pv_area = []
    filename = []
    output_filename = []
    columns = []

    def __init__(self, filename, output_filename, pv_efficiency, pv_area, columns):
        self.pv_efficiency = pv_efficiency
        self.pv_area = pv_area
        self.columns = columns

        self.filename = filename
        self.output_filename = output_filename

        self.read_file()
        loss = self.calculate_loss()
        self.add_global_outputs()
        self.plot_loss(loss)
        self.write_file()

    def read_file(self):
        labels = ['Time',
                  'Horizon Shading [kWh/m2]',
                  'Global Radiation at the Module [kWh/m2]',
                  'Global PV Radiation [kWh]',
                  'Rated PV Energy [kWh]',
                  'Module Shading [kWh]']

        self.df = pd.read_csv(self.filename, usecols=self.columns, header=None, names=labels, skiprows=18)

        index = range(0, len(self.df['Time']))
        self.df['Index'] = index
        self.df.set_index('Index')

    def write_file(self):
        self.df.to_csv(self.output_filename, columns=['Time',
                                                      'Horizon (Diffuse) Shading [kWh]',
                                                      'Module Shading [kWh]',
                                                      'PV Energy [kWh]',
                                                      'Loss Percent [%]',
                                                      'Loss Percent Diffuse [%]',
                                                      'Global Radiation at Module [kWh]',
                                                      'Global PV Radiation [kWh]',
                                                      'STC Conversion Fraction [0-1]'])

    def calculate_loss(self):

        pv_energy = self.df['Rated PV Energy [kWh]']
        module_shading = self.df['Module Shading [kWh]']
        horizon_shading = self.df['Horizon Shading [kWh/m2]'] * self.pv_area * self.pv_efficiency

        loss_percent = []
        loss_percent_diffuse = []

        pv_energy_no_diffuse = []
        pv_shaded = []
        loss_shade = []

        for i in range(0, len(pv_energy)):

            # remove diffuse horizon shading from PV
            pv_energy_no_diffuse.append(pv_energy[i] + abs(horizon_shading[i]))

            # compute "shaded" pv energy
            pv_shaded.append(pv_energy_no_diffuse[i] + horizon_shading[i] + module_shading[i])

            # total shade loss
            loss_shade.append(horizon_shading[i] + module_shading[i])

            # total loss percent
            if pv_energy[i] > 0:
                loss_percent.append(100 * abs(loss_shade[i])/pv_energy_no_diffuse[i])
            else:
                loss_percent.append(100)

            # diffuse loss percent
            loss_percent_diffuse.append(abs(horizon_shading[i])/pv_energy_no_diffuse[i])

            '''
            # partial shading
            if module_shading[i] < 0 or horizon_shading[i] < 0:
                loss_percent.append(100 * abs(module_shading[i] + horizon_shading[i]) / (pv_energy[i] + horizon_shading[i]) )
            else:
                # no shading
                if pv_energy[i] > 0:
                    loss_percent.append(0)
                # night time
                else:
                    loss_percent.append(100)
            '''

        self.df['Loss Percent [%]'] = loss_percent
        self.df['Loss Percent Diffuse [%]'] = loss_percent_diffuse
        self.df['Horizon (Diffuse) Shading [kWh]'] = horizon_shading
        self.df['PV Energy [kWh]'] = pv_energy_no_diffuse

        return loss_percent

    def add_global_outputs(self):

        self.df['Global Radiation at Module [kWh]'] = self.df['Global Radiation at the Module [kWh/m2]'] * self.pv_area
        stc = pd.Series(8760 * [self.pv_efficiency])
        self.df['STC Conversion Fraction [0-1]'] = stc.values

# currently only plots module losses, not anything related to horizon shading
    def plot_loss(self, loss):

        plt.style.use('fivethirtyeight')
        plt.plot(loss[0:23])
        plt.title('January 1 Module Shading Losses')
        plt.xlabel('Time [hour of day]')
        plt.ylabel('Shading Loss [%]')

        ax = plt.gca()
        ax.set_xlim([0, 24])
        ax.set_ylim([-2, 105])

        filename = self.filename
        plot_filename = filename.replace('.csv', '')
        plot_filename = plot_filename + " loss"

        plt.savefig(plot_filename, type='png', bbox_inches='tight')
        plt.close()
        #plt.show()


# Basic Tests
area = 1.5
efficiency = 0.1334
columns_1 = [0, 11, 13, 20, 23, 25]

test1 = process_pvsol('Basic Test 1.csv', 'test1_PVSOL.csv', efficiency, area, columns_1)
test2 = process_pvsol('Basic Test 2.csv', 'test2_PVSOL.csv', efficiency, area, columns_1)
test3 = process_pvsol('Basic Test 3.csv', 'test3_PVSOL.csv', efficiency, area, columns_1)

# Babbitt
area = 237.9
efficiency = 0.1178
columns_2 = [0, 8, 10, 11, 14, 16]
test4 = process_pvsol('9815 Babbitt.csv', 'babbitt_PVSOL.csv', efficiency, area, columns_2)

# Ivanhoe
area = 15.529125
efficiency = 0.0515
test5 = process_pvsol('Ivanhoe.csv', 'ivanhoe_PVSOL.csv', efficiency, area, columns_1)

# Halstead
area = 105.0229375
efficiency = 0.1333
test6 = process_pvsol('17339 Halstead.csv', 'halstead_PVSOL.csv', efficiency, area, columns_2)

# Trieu
area = 43.6
efficiency = 0.1193
test6 = process_pvsol('Trieu.csv', 'paradise_PVSOL.csv', efficiency, area, columns_2)

