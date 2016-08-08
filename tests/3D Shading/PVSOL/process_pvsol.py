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
        self.plot_loss(loss)
        self.write_file()

    def read_file(self):
        labels = ['Time',
                  'Horizon Shading [kWh/m2]',
                  'Rated PV Energy [kWh]',
                  'Module Shading [kWh]']

        self.df = pd.read_csv(self.filename, usecols=self.columns, header=None, names=labels, skiprows=18)

        index = range(0, len(self.df['Time']))
        self.df['Index'] = index
        self.df.set_index('Index')

    def write_file(self):
        self.df.to_csv(self.output_filename, columns=['Time',
                                                      'Horizon Shading [kWh/m2]',
                                                      'Module Shading [kWh]',
                                                      'Rated PV Energy [kWh]',
                                                      'Loss Percent [%]'])


    def calculate_loss(self):

        pv_energy = self.df['Rated PV Energy [kWh]']
        module_shading = self.df['Module Shading [kWh]']
        horizon_shading = self.df['Horizon Shading [kWh/m2]'] * self.pv_area * self.pv_efficiency

        loss_percent = []

        for i in range(0, len(pv_energy)):
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

        self.df['Loss Percent [%]'] = loss_percent
        return loss_percent

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
columns_1 = [0, 11, 23, 25]

#test1 = process_pvsol('Basic Test 1.csv', 'test1_PVSOL.csv', efficiency, area, columns_1)
#test2 = process_pvsol('Basic Test 2.csv', 'test2_PVSOL.csv', efficiency, area, columns_1)
#test3 = process_pvsol('Basic Test 3.csv', 'test3_PVSOL.csv', efficiency, area, columns_1)

# Babbitt
area = 237.9
efficiency = 0.1178
columns_2 = [0, 8, 14, 16]
#test4 = process_pvsol('9815 Babbitt.csv', 'test_babbitt_PVSOL.csv', efficiency, area, columns_2)

# Ivanhoe
area = 15.529125
efficiency = 0.0515
#test5 = process_pvsol('Ivanhoe.csv', 'test_ivanhoe_PVSOL.csv', efficiency, area, columns_1)

# Halstead
area = 105.0229375
efficiency = 0.1333
#test6 = process_pvsol('17339 Halstead.csv', 'test_halstead_PVSOL.csv', efficiency, area, columns_2)

# Trieu
area = 43.6
efficiency = 0.1193
test6 = process_pvsol('Trieu.csv', 'test_trieu_PVSOL.csv', efficiency, area, columns_2)
