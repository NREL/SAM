# End Use Load Profiles

The `eulp` Python package contains three modules that you can use in your code to work with individual building time series data from the <a href="https://data.openei.org/submissions/4520">OpenEI End Use Load Profiles (EULP) for the U.S. Building Stock</a>:

* `get`: Functions to download metadata, weather, and load files from the various datasets in the database.

* `plot`: Functions to generate plots of the downloaded data.

* `stats`: Functions to calculate total and peak load values from the downloaded data.

To use the package, copy the `eulp` folder to the folder containing your Python script, and import the package with:

```
from eulp import get, plot, stats
```

### Requirements:

```
datetime
os
re
colorcet==3.0.1
matplotlib==3.6.0
pandas==1.5.2
seaborn==0.12.1
urllib3==1.26.2
```

To installed required packages:

```
cd ../eulp
pip install -r requirements.txt
```

To test the package, run the `load-data-example.py` script, which includes instructions in a comment block at the top of the script.

Each of the modules in the [eulp](./eulp) folder is a self-contained script that you can use and adapt for your scripting projects.

## Sample Results

The [Sample Results](./Sample%20Results) folder contains files we downloaded for a residential building in Jefferson County, Colorado so you can see what the files look like. (We have omitted the .parquet files because they are too large to include in the repository.)

## Using EULP Data in SAM

When you run the `eulp.get.get_load_data()` function, it creates two CSV files of 15-minute load data for the building represented by the building ID `bldg_id`. The `load-data-all-[bldg_id].csv` file contains all of the appliance data and the total electric load data. The `load-data-total-[bldg_id]` contains only the total building electric load data.

To use the data in SAM:

1. On SAM's Electric Load page, click the **Edit array** button.

2. In the Edit Array window, click **Change time step** and type `15` for the time step in minutes.

3. Use a spreadsheet program to open the `load-data-all-[bldg_id].csv` file, and copy values from the "out.electricity.total.energy_consumption" column. There should be 35,040 rows of data.

4. In SAM's Edit Array window, click **Paste**.

You will need 15-minute weather data to run a simulation in SAM, which you can download by using the **Advanced download** option on the Location and Resource page, and choosing a 15-minute file from the "psm3-5min" dataset. You can choose the year to match the load data if you want to use SAM simulation results to explore the correlation between weather and load.

## Additional Resources

TMY3 weather files used to generate ResStock and ComStock data are available here: https://data.nrel.gov/submissions/156
