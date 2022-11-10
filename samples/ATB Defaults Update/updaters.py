import pandas as pd
import math

class BaseUpdater:
    """
    Base abstract update class, must be sub-classes to be used.
    """

    def __init__(self, tech_name, dollar_year):
        self.technology = tech_name
        self.year = 2020
        self.atb_year = 2022
        self.dollar_year = dollar_year

    def update_defaults(self, defaults_json, atb_df):
        # Query ATB DF
        tech_df = atb_df.query("Technology == @self.technology and atb_year == @self.atb_year and Year == @self.year")
        print(tech_df)
        
        # iterate over results
        for index, row in tech_df.iterrows():
            parameter = row["ParameterDetail"]
            sam_name = self.names.get(parameter, None)
            if sam_name is not None:
                value = row["Value"]
                units = row["Units"]
                if "$" in units:
                    value = self.inflate_value(value)
                value = self.round_value(value, units)
                print("new value", sam_name, value)
                defaults_json[sam_name] = value
        return defaults_json
    
    def inflate_value(self, value):
        if self.dollar_year == 2020:
            return value * 1.1087 # 2020$ to 2022$ (first half)
        elif self.dollar_year == 2021:
            return value * 1.0458 # 2020$ to 2022$ (first half)
    
    def round_value(self, value, units):
        if "$" in units:
            return round(value, 2)
        elif "%" in units:
            return round(value)
        else:
            return round(value, -1.0 * math.floor(math.log(value, 10)) - 6) # 6 sig figs, round uses opposite convention from log

class BasePVUpdater(BaseUpdater):

    names = {
        "Module" : "per_module",
        "Inverter" : "per_inverter",
        "Balance of system equipment" : "bos_equip_perwatt",
        "Installation labor" : "install_labor_perwatt",
        "Installer margin and overhead" : "install_margin_perwatt",
        "Contingency Percent" : "contingency_percent",
        "Engineering and developer overhead" : "engr_per_watt",
        "Grid interconnection" : "grid_per_watt",
        "Land prep & transmission" : "land_per_watt",
        "Permitting and environmental studies" : "permitting_per_watt",
        "Sales Tax Basis" : "sales_tax_percent",
        "Sales Tax Percent" : "sales_tax_value"
    }

    def __init__(self, tech_name):
        super().__init__(tech_name, 2021)
        self.year = 2022
        self.atb_year = 2023

class WindUpdater(BaseUpdater):

    # Names in the "parameter detail" row
    names = {
        "Turbine" : "turbine_cost_per_kw",
        "BOS" : "bos_cost_per_kw"
    }

    def __init__(self, tech_name):
        super().__init__(tech_name, 2020)

    def update_defaults(self, defaults_json, atb_df):
        defaults_json = super().update_defaults(defaults_json, atb_df)
        # Extract additional parameter
        tech_df = atb_df.query('Technology == @self.technology and atb_year == @self.atb_year and Year == @self.year and Parameter == "O&M"')
        if len(tech_df) > 0:
            print(tech_df["Value"].values[0])
            defaults_json["om_capacity"] = tech_df["Value"].values.tolist() # O&M specified as array
        return defaults_json

