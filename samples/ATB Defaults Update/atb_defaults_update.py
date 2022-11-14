import rapidjson
import os
import pandas as pd
from updaters import BasePVUpdater, WindUpdater

financial_models = {"All Equity Partnership Flip" : "Utility",
                    "Leveraged Partnership Flip" : "Utility",
                    "Sale Leaseback" : "Utility",
                    "Single Owner" : "Utility",
                    "Merchant Plant" : "Utility",
                    "Community" : "Commercial",
                    "Commercial" : "Commercial",
                    "Host Developer" : "Commercial",
                    "Residential" : "Residential"}

tech_models = {"Flat Plate PV" : {"Utility" : BasePVUpdater("UtilityPV"),
                                  "Commercial" : BasePVUpdater("CommPV"),
                                  "Residential" : BasePVUpdater("ResPV")},
               "PVWatts" : {"Utility" : BasePVUpdater("UtilityPV"),
                                  "Commercial" : BasePVUpdater("CommPV"),
                                  "Residential" : BasePVUpdater("ResPV")},                                 
                "Wind Power" : {"Utility" : WindUpdater("LandbasedWind")} # Distributed wind does not provide CAPEX breakdowns, do these by hand
                }

def update_defaults_file(file_name, atb_data):
    models = file_name.split(".")[0].split("_") # Truncate .json from the file name, split at _ to get models
    tech = tech_models.get(models[0], None)
    scale = financial_models.get(models[1], None)
    print(models)
    if scale is not None and tech is not None:
        updater = tech.get(scale, None)
        if updater is not None:
            defaults_data = {}
            with open(root + os.sep + file_name, "r") as file:
                defaults_data = rapidjson.loads(file.read(), number_mode=rapidjson.NM_DECIMAL)

            new_defaults, atb_data = updater.update_defaults(defaults_data, atb_data)

            with open(root + os.sep + file_name, "w") as file:
                json_string = rapidjson.dumps(new_defaults, indent=4, number_mode=rapidjson.NM_DECIMAL)
                json_string = json_string.replace("E+", "e")
                json_string = json_string.replace("E-", "e-")
                json_string = json_string.replace("e-0", "e-")
                file.write(json_string)

    return atb_data

## TODO: download parameters.csv from OEDI w/ URL
## Note: for 2022 we will be reading in multiple files - just comment stuff out or iterate over multiple?

atb_file_name = "C:\\Users\\bmirletz\\source\\repos\\SAM-documentation\\Defaults References and Tools\\Fall 2022\\PV.csv"
atb_parameter_data = pd.read_csv(atb_file_name )

printed = False

for root, dirs, files in os.walk("../../deploy/runtime/defaults"):
    for file in files:
        if ".json" in file:
            
            try:
                atb_parameter_data = update_defaults_file(file, atb_parameter_data)
                atb_parameter_data.to_csv(atb_file_name)
            except KeyError:
                print("KeyError in file ", file)