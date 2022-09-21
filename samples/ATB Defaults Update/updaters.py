import pandas as pd

class BaseUpdater:
    """
    Base abstract update class, must be sub-classes to be used.
    """

    def __init__(self, tech_name):
        self.technology = tech_name
        self.year = 2020
        self.atb_year = 2022

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
                # TODO: do we need to inflate values to 2022 dollars?
                print("new value", sam_name, value)
                defaults_json[sam_name] = value
        return defaults_json

class BasePVUpdater(BaseUpdater):

    names = {
        "Module" : "per_module",
        "Inverter" : "per_inverter",
        "Balance of system equipment" : "bos_equip_perwatt",
        "Installation labor" : "install_labor_perwatt",
        "Installer margin and overhead" : "install_margin_perwatt",
        "Contingency" : "contingency_percent",
        "Engineering and developer overhead" : "engr_per_watt",
        "Grid interconnection" : "grid_per_watt",
        "Land prep & transmission" : "land_per_watt",
        "Permitting and environmental studies" : "permitting_per_watt",
        "Sales Tax Basis" : "sales_tax_percent",
        "Sales Tax" : "sales_tax_value"
    }

    def __init__(self, tech_name):
        super().__init__(tech_name)
        self.year = 2021

class WindUpdater(BaseUpdater):

    # Names in the "parameter detail" row
    names = {
        "Turbine" : "turbine_cost_per_kw",
        "BOS" : "bos_cost_per_kw"
    }

    def __init__(self, tech_name):
        super().__init__(tech_name)

    def update_defaults(self, defaults_json, atb_df):
        defaults_json = super().update_defaults(defaults_json, atb_df)
        # Extract additional parameter
        tech_df = atb_df.query('Technology == @self.technology and atb_year == @self.atb_year and Year == @self.year and Parameter == "O&M"')
        print(tech_df["Value"].values[0])
        defaults_json["om_capacity"] = tech_df["Value"].values.tolist() # O&M specified as array
        return defaults_json

