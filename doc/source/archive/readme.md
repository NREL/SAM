# Archive

The archive folder contains files for sections of Help that were in older versions of SAM. These files are not included in the current version of Help.

The "Cash Flow Descriptions" section was removed after we implemented the Send to Excel With Equations feature and wrote the "Sample Spreadsheets" for the SAM website at https://sam.nlr.gov/financial-models.html.

The sections for Direct Steam Tower, ISCC, and Dish Stirling models were removed from Help when we retired those models from SAM.

The following `toctree` directives show the structure of these sections.

**Cash Flow Descriptions**

```
.. toctree::
   :maxdepth: 1
   :caption: Cash Flow Details

   cash_flow_variables
   cf_residential_and_commercial
   cf_single_owner
   cf_all_equity_partnership_flip
   cf_leveraged_partership_flip
   cf_sale_leaseback
```

**CSP Direct Steam Tower**

```
.. toctree::
   :maxdepth: 1
   :caption: CSP Direct Steam Tower

   power_tower_direct_steam
   dspt_heliostat_field
   dspt_tower_and_receiver
   dspt_power_cycle
   dspt_parasitics
```

**CSP Integrated Solar Combined Cycle**

```
.. toctree::
   :maxdepth: 1
   :caption: CSP ISCC

   iscc
   iscc_location_and_resource
   iscc_heliostat_field
   iscc_tower_receiver_cycle
   iscc_parasitics
   iscc_system_costs
```

**CSP Dish Stirling**

```
.. toctree::
   :maxdepth: 1
   :caption: CSP Dish Stirling

   dish_stirling
   dish_location_and_resource
   dish_solar_field
   dish_collector
   dish_receiver
   dish_stirling_engine
   dish_parasitics
   dish_reference_inputs
   dish_system_costs
```