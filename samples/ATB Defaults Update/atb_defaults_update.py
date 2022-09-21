import rapidjson
import os
import pandas as pd
from updaters import BasePVUpdater, WindUpdater

financial_models = {"All Equity Partnership Flip" : "Utility",
                    "Leveraged Partnership Flip" : "Utility",
                    "Sale Leaseback" : "Utility",
                    "Single Owner" : "Utility",
                    "Merchant Plant" : "Utility",
                    "Community" : "Utility",
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
"""

tech_models = {"Wind Power" : {"Utility" : WindUpdater("LandbasedWind")}}

class ExponentEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, float):
            return 
"""

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
                defaults_data = rapidjson.loads(file.read())

            new_defaults = updater.update_defaults(defaults_data, atb_data)

            with open(root + os.sep + file_name, "w") as file:
                rapidjson.dump(new_defaults, file, indent=4)

## TODO: download parameters.csv from OEDI w/ URL

atb_parameter_data = pd.read_csv("Parameter2022.csv")

printed = False

for root, dirs, files in os.walk("../../deploy/runtime/defaults"):
    for file in files:
        if ".json" in file:
            
            try:
                update_defaults_file(file, atb_parameter_data)
            except KeyError:
                print("KeyError in file ", file)