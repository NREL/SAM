Release Notes
~~~~~~~~~~~~~

These are the software release notes for the System Advisor Model™ (SAM™) developed by the Department of Energy's National Laboratory of the Rockies (NLR).

.. _2026-7-3:

SAM 2026.7.3, SSC 308: July 3, 2026
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the first version we have released as the National Laboratory of the Rockies (formerly the National Renewable Energy Laboratory).

This version updates Geothermal model costs and power block input options.

Other highlights include:

* Weather Data

  * Downloads from the National Solar Radiation Database (NSRDB) include new data for the Arctic region.
  * Photovoltaic models can download snow depth data for U.S. locations for the snow loss model.
  * The Location and Resource page is reorganized for better usability.

* Photovoltaic Models

  * The full battery model is available for PVWatts.
  * The Detailed Photovoltaic model has a new look and options for spectral correction.

* Financial Models

  * ITC qualifying costs can be based on only battery-related costs for PV systems, on specific components of a hybrid system, or on a percentage of the total installed cost.
  * The Commercial financial model uses the same detailed depreciation inputs as the Single Owner and other front-of-meter financial models.
  * The Single Owner financial model has a new option for non-energy revenue to support projects like agrivoltaics.

* CSP models

  * The Physical Trough model has a new two-dimensional optical model.
  * The Molten Salt Power Tower model has a new option (in Beta) to control the plant for net plant electricity output.

For a complete list of code revisions associated with this version, see the `SAM <https://github.com/NatLabRockies/SAM/pulls?q=is%3Apr+milestone%3A%22SAM+2026+Release%22+is%3Aclosed>`__ and `SSC <https://github.com/NatLabRockies/ssc/pulls?q=is%3Apr+milestone%3A%22SAM+2026+Release%22+is%3Aclosed>`__ GitHub repositories.

General (SAM 2026.7.3)
----------------------

Add support for day of week so time-dependent time series input data can synchronize with weather data. (`SAM 2109 <https://github.com/NatLabRockies/SAM/pull/2109>`__, `SSC 1352 <https://github.com/NatLabRockies/SAM/pull/1352>`__)

Update Solar Resource downloads for latest National Solar Radiation Database (NSRDB) datasets, including adding access to NSRDB Polar datasets for arctic regions. (`SAM 2173 <https://github.com/NatLabRockies/SAM/pull/2173>`__)

Update Solar Resource File Converter macro to handle recent changes to SolarAnywhere format. (`SAM 2170 <https://github.com/NatLabRockies/SAM/pull/2170>`__)

Move Help documentation to open source platform. (`SAM 2145 <https://github.com/NatLabRockies/SAM/pull/2145>`__, `SAM 2141 <https://github.com/NatLabRockies/SAM/pull/2141>`__)

Add better support for running hybrid system model from SSC Software Development Kit and PySAM. (`SSC 1356 <https://github.com/NatLabRockies/SAM/pull/1366>`__)

Add new LK script `export_cashflow_excel()` function to allow access to SAM's **Send to Excel with Equations** feature from LK scripts (Windows only), and add support for tables with two header rows in PDF reports. (`SAM 2121 <https://github.com/NatLabRockies/SAM/pull/2121>`__)

Fix problem with navigation menu being out of sync with input pages. (`SAM 2100 <https://github.com/NatLabRockies/SAM/issues/2100>`__)

Financial Models (SAM 2026.7.3)
-------------------------------

Add option to Single Owner financial model for non-energy revenue streams. (`SAM 2190 <https://github.com/NatLabRockies/SAM/pull/2190>`__, `SAM 2194 <https://github.com/NatLabRockies/SAM/pull/2194>`__, `SSC 1398 <https://github.com/NatLabRockies/SAM/pull/1398>`__, `SSC 1387 <https://github.com/NatLabRockies/SAM/pull/1397>`__)

Add option for determining ITC qualifying costs and depreciation basis based on system components to existing option based on percentage of total installed cost, and update Commercial fiancial model to use the same detailed depreciation inputs as the front-of-meter (FOM) financial models. (`SAM 2187 <https://github.com/NatLabRockies/SAM/pull/2187>`__, `SSC 1395 <https://github.com/NatLabRockies/SAM/pull/1395>`__, `SSC 1376 <https://github.com/NatLabRockies/SAM/pull/1376>`__, `SSC 1370 <https://github.com/NatLabRockies/SAM/pull/1370>`__)

Update default PPA price and retail electricity rates to account for reduction in default incentives. (`SAM 2217 <https://github.com/NatLabRockies/SAM/pull/2217>`__)

Fix errors for Send-to-Excel-with-equations for Community Solar financial model. (`SAM 2169 <https://github.com/NatLabRockies/SAM/pull/2169>`__)

Fix issue with curtailment input array. (`SAM 2139 <https://github.com/NatLabRockies/SAM/pull/2139>`__, `SSC 1366 <https://github.com/NatLabRockies/SAM/pull/1366>`__)

Fix Merchant Plant problem with incorrect cash flow revenue when market prices assigned using the Lifetime Matrix UI widget in Annual mode. (`SSC 1389 <https://github.com/NatLabRockies/SAM/pull/1389>`__)

Battery Storage (SAM 2026.7.3)
------------------------------

Fix front-of-meter (FOM) battery automatic dispatch with custom forecast option for Custom Generation - Battery configurations. (`SAM 2211 <https://github.com/NatLabRockies/SAM/pull/2211>`__)

Update Battery Compare Cases macro to report battery roundtrip efficiency and fix metrics for standalone battery. (`SAM 2176 <https://github.com/NatLabRockies/SAM/pull/2176>`__)

Add option to discharge to grid for behind-the-meter (BTM) retail rates battery dispatch option. (`SAM 2158 <https://github.com/NatLabRockies/SAM/pull/2158>`__)

Allow charge from grid limited system power for manual battery dispatch option. (`SAM 2115 <https://github.com/NatLabRockies/SAM/pull/2187>`__, `SSC 1348 <https://github.com/NatLabRockies/SAM/pull/1348>`__)

Fix problem with battery target power and current available at a given state of charge. (`SSC 1362 <https://github.com/NatLabRockies/SAM/pull/1362>`__)

Fix problem that caused a crash when running battery model from PySAM. (`SSC 1347 <https://github.com/NatLabRockies/SAM/pull/1347>`__)

Detailed Photovoltaic (2026.7.3)
--------------------------------

Update module and inverter libraries to data available from California Energy Commission (CEC) equipment lists from June 23, 2026. Also migrated module library generation process to use Python script instead of LK script. (`SAM 2233 <https://github.com/NatLabRockies/SAM/pull/2233>`__)

Reorganize user interface input pages to improve usability. (`SAM 2135 <https://github.com/NatLabRockies/SAM/pull/2135>`__, `SAM 2120 <https://github.com/NatLabRockies/SAM/pull/2120>`__)

Add support for downloading snow depth data from online databases. (`SAM 2089 <https://github.com/NatLabRockies/SAM/pull/2089>`__, `SSC 1335 <https://github.com/NatLabRockies/SAM/pull/1335>`__)

Add option to choose a spectral correction model on the Module page. This adds the `spectral_correction_model_choice` input variable to SSC and PySAM with options 0=Air Mass Only (De Soto), 1=Air mass and precipitable water (Lee), 2=Air mass and clearsky index (Pelland). (`SAM 2203 <https://github.com/NatLabRockies/SAM/pull/2203>`__, `SSC 1406 <https://github.com/NatLabRockies/ssc/pull/1406>`__)

Fix PV uncertainty inputs to prevent invalid P-values. (`SAM 2160 <https://github.com/NatLabRockies/SAM/pull/2160>`__)

Add sample LK script to generate a photovoltaic losses bar graph. (`SAM 2142 <https://github.com/NatLabRockies/SAM/pull/2142>`__)

Update PVsyst OND - PAN to SAM macro to improve error messages and better handle parameters not provided in PAN files. (`SAM 2112 <https://github.com/NatLabRockies/SAM/pull/2112>`__)

Improve Detailed PV and PVWatts albedo error messages. (`SSC 1385 <https://github.com/NatLabRockies/SAM/pull/1385>`__)

Fix memory problem with snow data. (`SSC 1377 <https://github.com/NatLabRockies/ssc/pull/1377>`__)

Improve module six-parameter solver. (`SSC 1373 <https://github.com/NatLabRockies/SAM/pull/1373>`__)

Fix issues with sky diffuse shading calculations in photovoltaic shading model. (`SSC 1355 <https://github.com/NatLabRockies/SAM/pull/1355>`__)

Fix issues with System Sizing macro. (`SAM 2103 <https://github.com/NatLabRockies/SAM/pull/2103>`__)

PVWatts (SAM 2026.7.3)
----------------------

Move bifacial input from Advanced Parameters section to make it more obvious. (`SAM 2200 <https://github.com/NatLabRockies/SAM/pull/2200>`__)

Use detailed battery model for PVWatts configurations to allow for access to all battery features. (`SAM 2105 <https://github.com/NatLabRockies/SAM/pull/2105>`__)

Change PVWatts simulation from single year to lifetime (over analysis period) for consistency with other performance models and to allow PVWatts model to run with the full battery model instead of the simple model. This changes PVWatts annual degradation from compounding to linear. (`SAM 2113 <https://github.com/NatLabRockies/SAM/pull/2113>`__)

For PVWatts-Battery configurations, add battery replacements to cash flow, and fix some default values. (`SAM 2185 <https://github.com/NatLabRockies/SAM/pull/2185>`__)

For PVWatts-Battery configurations, allow module area estimate for land area calculations. (`SAM 2107 <https://github.com/NatLabRockies/SAM/pull/2107>`__)

Geothermal (SAM 2026.7.3)
-------------------------

Update geothermal model costs and improve power block input options. (`SAM 2198 <https://github.com/NatLabRockies/SAM/pull/2198>`__, `SSC 1402 <https://github.com/NatLabRockies/SAM/pull/1402>`__)

Marine Energy (SAM 2026.7.3)
----------------------------

Fix JPD generation and update cost documentation. (`SAM 2197 <https://github.com/NatLabRockies/SAM/pull/2197>`__)

Update wave resource input page to improve usability and add Puerto Rico data downloads. (`SAM 2128 <https://github.com/NatLabRockies/SAM/pull/2128>`__, `SSC 1357 <https://github.com/NatLabRockies/SAM/pull/1357>`__, `SSC 1401 <https://github.com/NatLabRockies/SAM/pull/1401>`__)

Concentrating Solar Power (SAM 2026.7.3)
----------------------------------------

Add options to physical trough model Collectors input page for specifying solar position, field incidence table, or incidence angle modifier coefficients. Previous versions only supported incidence angle modifier coefficients. (`SAM 2182 <https://github.com/NatLabRockies/SAM/pull/2182>`__, `SAM 2195 <https://github.com/NatLabRockies/SAM/pull/2195>`__, `SSC 1392 <https://github.com/NatLabRockies/SAM/pull/1392>`__)

Update sCO2 Python script and change log. (`SAM 2118 <https://github.com/NatLabRockies/SAM/pull/2118>`__, `SSC 1353 <https://github.com/NatLabRockies/SAM/pull/1353>`__, `SAM 2127 <https://github.com/NatLabRockies/SAM/pull/2127>`__)

Fix CSP model adjustment factor indexing. (`SSC 1369 <https://github.com/NatLabRockies/SAM/pull/1369>`__)

Add net power control option for the molten salt power tower model, available on the System Design page. (`SAM 2189 <https://github.com/NatLabRockies/SAM/pull/2189>`__, `SSC 1396 <https://github.com/NatLabRockies/ssc/pull/1396>`__)

Fix various bugs in physical trough model. (`SAM 1393 <https://github.com/NatLabRockies/ssc/pull/1393>`__)

Add back work ratio and turbine sums to sCO2 model outputs. (`SSC 1360 <https://github.com/NatLabRockies/ssc/pull/1360>`__)

Wind Power (SAM 2026.7.3)
-------------------------

Update wind resource downloads to latest data from WIND Toolkit. (`SAM 2202 <https://github.com/NatLabRockies/SAM/pull/2202>`__)

Change wind power simulation from single year to lifetime (over analysis period) for consistency with other performance models. This changes wind power annual degradation from compounding to linear. (`SAM 2174 <https://github.com/NatLabRockies/SAM/pull/2174>`__, `SSC 1386 <https://github.com/NatLabRockies/SSC/pull/1386>`__)

Remove Siting Considerations macro due due to discontinued web service. (`SAM 2165 <https://github.com/NatLabRockies/SAM/pull/2165>`__)

Remove deprecated LandBOSSE Python models. (`SAM 2154 <https://github.com/NatLabRockies/SAM/pull/2154>`__, `SSC 1375 <https://github.com/NatLabRockies/SAM/pull/1375>`__)

Custom Generation Profile (SAM 2026.7.3)
----------------------------------------

Add option to use lifetime (over analysis period) generation data as input. (`SAM 2166 <https://github.com/NatLabRockies/SAM/pull/2166>`__, `SSC 1384 <https://github.com/NatLabRockies/SSC/pull/1384>`__)

Fuel Cell (SAM 2026.7.3)
------------------------

Add warnings when fuel cell dispatch time-of-delivery (TOD) periods are incorrect. (`SAM 2196 <https://github.com/NatLabRockies/SAM/pull/2196>`__, `SSC 1400 <https://github.com/NatLabRockies/SSC/pull/1400>`__)

Developer Updates (2026.7.3)
----------------------------

Integrate Google OR-Tools to provide access to optimization libraries. (`SAM 2146 <https://github.com/NatLabRockies/SAM/pull/2146>`__)

Update user interface platform to wxWidgets 3.2.8. (`SAM 2114 <https://github.com/NatLabRockies/SAM/pull/2114>`__, `WEX 190 <https://github.com/NatLabRockies/wex/pull/190>`__)

.. _2025-4-16-r2:

SAM 2025.4.16 Revision 2, SSC 306: April 21, 2026
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This maintenance update incorperates new URLs to reflect the laboratory name change as well as a few other minor fixes.

- Update API calls to NLR.gov. (`SAM 2157 <https://github.com/NatLabRockies/SAM/pull/2157>`__)
- Add an input array for the hourly turbine fraction to the Molten Salt Power Tower model. (`SSC 1349 <https://github.com/NatLabRockies/ssc/pull/1349>`__)
- Fix battery segfault on an empty array. (`SSC 1347 <https://github.com/NatLabRockies/ssc/pull/1347>`__)
- Allow module area claculation for PVWatts Battery configurations. (`SAM 2107 <https://github.com/NatLabRockies/SAM/pull/2107>`__)

.. _2025-4-16-r1:

SAM 2025.4.16 Revision 1, SSC 303: July 24, 2025
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This maintenance update fixes a weather file download bug and several other issues.

- Fix crash caused when attempting to download a weather file using a location name or street address, or when downloading a static map image underlay for the photovoltaic model's 3D shade calculator. The time zone output of the LK geocode function is now optional. An outstanding task for a future release is to identify new APIs for time zone and static map. For this update, the LK geocode function does not return a time zone, and the 3D shade calculator static map download does not work -- a workaround is to import a map image file instead. (`SAM 2093 <https://github.com/NatLabRockies/SAM/pull/2093>`__)
- Fix Append Snow Data macro for photovoltaic models for snow depth data in inches. (`SAM 2079 <https://github.com/NatLabRockies/SAM/pull/2079>`__)
- Fix issue with photovoltaic cell temperature heat transfer method with array dimensions option. (`SSC 1331 <https://github.com/NatLabRockies/ssc/pull/1331>`__)
- Fix issue with Parabolic Trough and Linear Fresnel IPH models solar field thermal power calculation. (`SSC 1329 <https://github.com/NatLabRockies/ssc/pull/1329>`__)
- Restore yellow "sticky note" icon to navigation menu for input pages that have a non-empty note. This stopped working when we implemented a multi-level navigation menu. (`SAM 2091 <https://github.com/NatLabRockies/SAM/pull/2091>`__, `WEX 188 <https://github.com/NatLabRockies/wex/pull/188>`__)
- Fix parametrics Create New Case feature when parametrics table has no results. (`SAM 2088 <https://github.com/NatLabRockies/SAM/pull/2088>`__)
- Fix issue with "Save all output variables over analysis period" option for cases with grid outages. (`SAM 2087 <https://github.com/NatLabRockies/SAM/pull/2087>`__)
- Fix defaults manager handling of JSON NaN values. This issue only affects SAM developers working with the SAM IDE. (`SAM 2086 <https://github.com/NatLabRockies/SAM/pull/2086>`__)

.. _2025-4-16:

SAM 2025.4.16, SSC 302: April 16, 2025
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version updates the geothermal power model with more drilling cost detail, improved capital and O&M cost models, and improved PySAM geothermal functionality. It also adds an option for a counterflow heat exchanger to SAM's Industrial Process Heat (IPH) models for a more realistic representation of the thermal load, and includes several improvements to the battery dispatch and degradation calculations, including a new option to allow the battery to discharge to grid for retail rates dispatch, among other improvements and bug fixes.

For a complete list of code revisions associated with this version, see the `SAM <https://github.com/NatLabRockies/SAM/pulls?q=is%3Apr+milestone%3A%22SAM+Spring+2025+Release%22+is%3Aclosed>`__ and `SSC <https://github.com/NatLabRockies/ssc/pulls?q=is%3Apr+milestone%3A%22SAM+Spring+2025+Release%22+is%3Aclosed>`__ GitHub repositories.

General (SAM 2025.4.16)
-----------------------

Update API call for electricity rate download-by-zip-code option. API now requires geocoding zip code to latitude/longitude before API call. (`SAM 2053 <https://github.com/NatLabRockies/SAM/pull/2053>`__)

Fix heat map for data with sub-hourly time steps. (`WEX 183 <https://github.com/NatLabRockies/wex/pull/183>`__)

Add support for not-a-number (NaN) and infinity (Inf) in JSON files. (`SAM 2047 <https://github.com/NatLabRockies/SAM/pull/2047>`__)

Add support for unicode characters in LK 'isdigit()', 'isalpha()', and 'isalnum()' functions. (`LK 54 <https://github.com/NatLabRockies/lk/pull/54>`__)

Geothermal Power (SAM 2025.4.16)
--------------------------------

Reorganize and clean up geothermal cost inputs. (`SAM 2045 <https://github.com/NatLabRockies/SAM/pull/2045>`__)

Update cost and well count calculations. (`SSC 1307 <https://github.com/NatLabRockies/ssc/pull/1307>`__)

Convert stimulation cost per well from input to calculated value. (`SAM 2063 <https://github.com/NatLabRockies/SAM/pull/2063>`__

Photovoltaics (SAM 2025.4.16)
-----------------------------

*The changes in this section apply to Detailed PV, PVWatts, PV-battery, and hybrid configurations with PV.*

Fix problem with capital cost curve equations for out-of-range system capacities. (`SAM 2044 <https://github.com/NatLabRockies/SAM/pull/2044>`__)

Fix default installation cost for PV residential PV configurations. (`SAM 2010 <https://github.com/NatLabRockies/SAM/pull/2010>`__)

Remove land lease cost inputs and outputs from behind-the-meter configurations. (`SAM 2003 <https://github.com/NatLabRockies/SAM/pull/2003>`__, `SAM 1997 <https://github.com/NatLabRockies/SAM/pull/1997>`__)

Remove error messages about bifacial modules and one-axis tracking. (`SAM 1311 <https://github.com/NatLabRockies/ssc/pull/1311>`__)

Detailed PV (SAM 2025.4.16)
---------------------------

Enable power consumption during operation for Inverter Datasheet option when weighted efficiency option is used. (`SAM 2066 <https://github.com/NatLabRockies/SAM/pull/2066>`__)

Account for inverter nighttime consumption in grid limits equations. Applies to configurations with Detailed PV. (`SAM 1277 <https://github.com/NatLabRockies/ssc/pull/1277>`__)

Revise PDF report template to show module type instead of a number. Applies to Detailed PV and PV-Battery only. (`SAM 2001 <https://github.com/NatLabRockies/SAM/pull/2001>`__)

Concentrating Solar Power (CSP) (SAM 2025.4.16)
-----------------------------------------------

Add two new SENER collectors to collector library for physical trough model. (`SAM 2029 <https://github.com/NatLabRockies/SAM/pull/2029>`__ )

Update default volume oversize value for trough packed bed TES model. The new default value allows the case to hit the design TES charge capacity. (`SAM 2054 <https://github.com/NatLabRockies/SAM/pull/2054>`__)

Fix boundary condition equations for packed bed TES model. Fix reporting unit bug for packed bed outputs. (`SSC 1312 <https://github.com/NatLabRockies/ssc/pull/1312>`__)

Industrial Process Heat (IPH) (SAM 2025.4.16)
---------------------------------------------

Add counterflow heat exchanger option to model heat transfer between the heat transfer fluid and steam heat sink. (`SAM 2002 <https://github.com/NatLabRockies/SAM/pull/2002>`__, `SSC 1274 <https://github.com/NatLabRockies/ssc/pull/1274>`__)

Update default solar field temperatures for IPH Parabolic Trough configurations for oil field HTF and salt storage fluid. (`SAM 1992 <https://github.com/NatLabRockies/SAM/pull/1992>`__)

Add new input for over-design mass flow rate. (`SAM 2046 <https://github.com/NatLabRockies/SAM/pull/2046>`__, `SSC 1308 <https://github.com/NatLabRockies/ssc/pull/1308>`__)

Electric Battery Storage (SAM 2025.4.16)
----------------------------------------

Fix UI checks for critical load length to work with Custom Generation Battery configurations. (`SAM 2049 <https://github.com/NatLabRockies/SAM/pull/2049>`__)

Add new time series output variable for manual dispatch period. (`SAM 1298 <https://github.com/NatLabRockies/ssc/pull/1298>`__)

Prevent negative "battery to load" values when losses exceed battery discharge power. (`SAM 1294 <https://github.com/NatLabRockies/ssc/pull/1294>`__)

Correct battery state of charge (SOC) calculation at the end of a grid outage period. (`SAM 1278 <https://github.com/NatLabRockies/ssc/pull/1278>`__)

Improve behavior of stacked-area plots on Time Series tab of Results page. (`SAM 1987 <https://github.com/NatLabRockies/SAM/pull/1987>`__)

Fix numerical error in Li-ion NMC/Graphite battery lifetime code that caused unexpected annual charge energy values of zero. (`SAM 1310 <https://github.com/NatLabRockies/ssc/pull/1310>`__)

Add support for exporting battery power to the grid for BTM retail rates dispatch option. (`SAM 2041 <https://github.com/NatLabRockies/SAM/pull/2041>`__, `SSC 1303 <https://github.com/NatLabRockies/ssc/pull/1303>`__)

Fix typo in internal 'batt_adjust_constant' variable name. (`SAM 1301 <https://github.com/NatLabRockies/ssc/pull/1301>`__)

Improve error handling of forecast price signals object to prevent SSC crashes for SAM Software Development Kit (SDK), PySAM, and gtest. (`SAM 1288 <https://github.com/NatLabRockies/ssc/pull/1288>`__)

Marine Energy (SAM 2025.4.16)
-----------------------------

Update cost curves. (`SAM 2043 <https://github.com/NatLabRockies/SAM/pull/2043>`__)

Fix conversion of time-series wave resource data to joint probability distribution (JPD) for files with three-hour data for over three years. (`SAM 2043 <https://github.com/NatLabRockies/SAM/pull/2043>`__)

Add cost curve for RM2 wave energy converter. (`SSC 1305 <https://github.com/NatLabRockies/ssc/pull/1305>`__)

Wind Power (SAM 2025.4.16)
--------------------------

Fix double-counting of time series adjustment factors. (`SAM 1284 <https://github.com/NatLabRockies/ssc/pull/1284>`__)

Remove wind file reader check on elevation. (`SSC 1317 <https://github.com/NatLabRockies/ssc/pull/1317>`__)

Custom Generation Profile (SAM 2025.4.16)
-----------------------------------------

Fix problem with combining generation profiles from cases with different weather file time steps. (`SAM 2012 <https://github.com/NatLabRockies/SAM/pull/2012>`__)

Financial Models (SAM 2025.4.16)
--------------------------------

Provide feedback for behind-the-meter (BTM) with billing demand enabled when "Included in Billing Demand" is zero for all periods. (`SSC 1293 <https://github.com/NatLabRockies/ssc/pull/1293>`__)

Remove duplicate electricity bill output variables. (`SAM 2009 <https://github.com/NatLabRockies/SAM/pull/2009>`__, `SSC 1276 <https://github.com/NatLabRockies/ssc/pull/1276>`__)

Add missing fuel cell O&M cost line item to cashflow and fix Send-to-Excel with Equations callback error for Fuel Cell / Commercial configuration. (`SAM 1995 <https://github.com/NatLabRockies/SAM/pull/1995>`__)

Change Commercial financial model IRR units from $ to %. (`SSC 1318 <https://github.com/NatLabRockies/ssc/pull/1318>`__)

Hybrid Systems (SAM 2025.4.16)
------------------------------

Fix "Apply values from library" button for time-of-delivery (TOD) factors on Revenue input page. (`SAM 1999 <https://github.com/NatLabRockies/SAM/pull/1999>`__)

.. _2024-12-12:

SAM 2024.12.12, SSC 298: December 12, 2024
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version adds the Single Owner and Commercial financial models as options to the Industrial Process Heat (IPH) configurations.

Other highlights include:

- For behind-the-meter (BTM) batteries, add new graphs to Summary tab of Results page showing monthly electricity bill components and energy to load, and on Time Series tab, show electricity to load in stacked area plots.
- New "Compare Cases" macro to generate a table comparing key metrics for different cases in a file that have the same financial model.
- Rename "Generic System" performance model to "Custom Generation Profile".
- New packed bed thermocline and pressurized water cylinder with piston separator TES options for the Physical Trough CSP and IPH performance models.
- Update NREL National Solar Radiation Database (NSRDB) downloads to access new GOES V4 (1998 - 2023) dataset.
- Speed up simulation run times of Detailed PV and PVWatts performance models.
- Updated PV module and inverter libraries.

For a complete list of code revisions associated with this version, see the `SAM <https://github.com/NatLabRockies/SAM/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2024+Release%22+is%3Aclosed>`__ and `SSC <https://github.com/NatLabRockies/ssc/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2024+Release%22+is%3Aclosed>`__ GitHub repositories.

General (SAM 2024.12.12)
------------------------

Update weather file downloads to NREL NSRDB GOES V4 dataset covering 1998 - 2023. (`SAM 1945 <https://github.com/NatLabRockies/SAM/pull/1945>`__)

Fix alignment of user interface elements for Linux and Mac versions of SAM. (`SAM 1926 <https://github.com/NatLabRockies/SAM/pull/1926>`__, `SAM 1929 <https://github.com/NatLabRockies/SAM/pull/1929>`__)

Fix code generator for MATLAB problem with incorrect library. (`SAM 1950 <https://github.com/NatLabRockies/SAM/pull/1950>`__)

Test and update macros and sample LK scripts. (`SAM 1948 <https://github.com/NatLabRockies/SAM/pull/1948>`__, `SAM 1956 <https://github.com/NatLabRockies/SAM/pull/1956>`__)

Test and update PDF report templates. (`SAM 1947 <https://github.com/NatLabRockies/SAM/pull/1947>`__)

Revise Help for new features, updates, and to correct errors. (`SAM 1960 <https://github.com/NatLabRockies/SAM/pull/1960>`__, `SAM 1941 <https://github.com/NatLabRockies/SAM/pull/1941>`__, `SAM 1919 <https://github.com/NatLabRockies/SAM/pull/1919>`__)

Fix problem with list of time step options in user interface widget for availability losses and time series data. (`SAM 1907 <https://github.com/NatLabRockies/SAM/pull/1907>`__)

Update sample Python script for accessing NREL ResStock building load data to 2024 datasets. (`SAM 1915 <https://github.com/NatLabRockies/SAM/pull/1915>`__)

New "Compare Cases" and "Compare Battery Cases" macros generate an HTML window that can be exported to PDF showing a table comparing key metrics for different cases that use the same financial model. (`SAM 1894 <https://github.com/NatLabRockies/SAM/pull/1894>`__)

Update NREL Cambium market forecast price data downloads to Cambium 2023. (`SAM 1889 <https://github.com/NatLabRockies/SAM/pull/1889>`__)

Fix Utility Rate Database (URDB) electricity rate downloads. (`SAM 1862 <https://github.com/NatLabRockies/SAM/pull/1862>`__)

Use consistent labels for monthly and annual energy variables. (`SSC 1236 <https://github.com/NatLabRockies/ssc/pull/1236>`__)

Improve display of parametric results that include "NaN" or "Inf" values. (`WEX 180 <https://github.com/NatLabRockies/wex/pull/180>`__)

Fix time series data viewer issue with leap days that caused time series to end on a day other than December 31. (`WEX 179 <https://github.com/NatLabRockies/wex/pull/179>`__)

Update Default Costs and System Designs (SAM 2024.12.12)
--------------------------------------------------------

Update defaults to better align with current state of U.S. market and incentives. (`SAM 1930 <https://github.com/NatLabRockies/SAM/pull/1930>`__)

For Detailed PV and PVWatts configurations, update default system designs based on `DOE Solar Photovoltaic System Cost Benchmarks <https://www.energy.gov/eere/solar/solar-photovoltaic-system-cost-benchmarks>`__:

- Behind-the-meter residential is 7.9 kWdc fixed roof-mount with 400 Wdc monofacial modules and 7.6 kWac string inverters with DC optimizers.
- Behind-the-meter commercial and third party ownership is 560 kWdc fixed or rack-mounted with 530 Wdc bifacial modules and 227 kWac central inverters. It is based on both the 3 MW agrivoltaic (APV) system from the `DOE Solar Photovoltaic System Cost Benchmarks <https://www.energy.gov/eere/solar/solar-photovoltaic-system-cost-benchmarks>`__ and the 200 kW fixed roof-mount system from the `2024 Annual Technology Baseline (ATB) <https://atb.nrel.gov/electricity/2024/data>`__.
- Front-of-meter is 100 MWdc ground-mounted, single-axis tracking with 530 Wdc bifacial modules and 2.5 MWac central inverters.
- PVWatts default system designs assume monofacial modules because bifacial option is hidden in collapsed "Advanced Parameters" panel. See `SAM Issue 1959 <https://github.com/NatLabRockies/SAM/issues/1959>`__.

For IPH configurations, update default incentives (no investment tax credit, ITC) and O&M cost. For Physical Trough, change default design to an oil field HTF with molten salt TES. For Fresnel, change default design to 5 MWt heat sink. (`SAM 1946 <https://github.com/NatLabRockies/SAM/pull/1946>`__)

Update default utility rate for behind-the-meter (BTM) configurations. (`SAM 1958 <https://github.com/NatLabRockies/SAM/pull/1958>`__)

Financial Models (SAM 2024.12.12)
---------------------------------

Update investment-tax credit (ITC) and depreciation basis calculations to remove financing fees and reserve account funding from basis. (`SAM 1860 <https://github.com/NatLabRockies/SAM/pull/1860>`__, `SSC 1193 <https://github.com/NatLabRockies/ssc/pull/1193>`__)

Add option for DSCR debt sizing option (debt-service coverage ration as input, or sculpted debt, sizing based on cash available for debt service) that was available only for Single Owner financial model to other front-of-meter financial models. (`SAM 1854 <https://github.com/NatLabRockies/SAM/pull/1854>`__, `SSC 1208 <https://github.com/NatLabRockies/ssc/pull/1208>`__)

Add internal rate of return (IRR) metric to Residential and Commercial financial models. Note that the value of these projects is based on electricitiy bill savings rather than revenue. (`SSC 1223 <https://github.com/NatLabRockies/ssc/pull/1223>`__)

Add nominal discount rate output variable to financial models to help with analyses involving net present value (NPV). (`SSC 1223 <https://github.com/NatLabRockies/ssc/pull/1223>`__)

Electricity Bill Calculator (SAM 2024.12.12)
--------------------------------------------

Fix downloads from Utility Rate Database (URDB) for utility companies with more than 500 rates (PG&E and others) or with no rates. (`SAM 1875 <https://github.com/NatLabRockies/SAM/pull/1873>`__)

Add option for net billing with carryover for credits that expire at end of true-up period instead of carrying over to following year, and credits apply to energy charge portion of monthly electricity bill. (`SAM 1835 <https://github.com/NatLabRockies/SAM/pull/1835>`__, `SSC 1200 <https://github.com/NatLabRockies/ssc/pull/1200>`__)

Improve error checking of time-of-use (TOU) rates by requiring that tiers are the same within a month across TOU periods. (`SSC 1207 <https://github.com/NatLabRockies/ssc/pull/1207>`__)

Fix an issue with front-of-meter (FOM) electricity purchases with hourly buy rates of zero. (`SSC 1264 <https://github.com/NatLabRockies/ssc/pull/1264>`__)

Detailed PV (SAM 2024.12.12)
----------------------------

Improve solar position algorithm to speed up PVWatts and Detailed PV simulation run times. (`SSC 1212 <https://github.com/NatLabRockies/ssc/pull/1212>`__)

Update module and inverter libraries. (`SAM 1913 <https://github.com/NatLabRockies/SAM/pull/1913>`__, `SAM 1917 <https://github.com/NatLabRockies/SAM/pull/1917>`__)

Rename temperature coefficient of power variable from 'gamma_r' to 'gamma_pmp' to be more consistent with industry standard. (`SSC 1231 <https://github.com/NatLabRockies/ssc/pull/1231>`__, `SAM 1898 <https://github.com/NatLabRockies/SAM/pull/1898>`__)

Add PV output distribution clipping correction method ("Walker method") with a new option on the Losses input page to choose between the matrix lookup (Allen) or PV output distribution (Walker) methods for hourly simulations. (`SAM 1861 <https://github.com/NatLabRockies/SAM/pull/1861>`__)

Fix six-parameter module coefficient generator sanity check that caused a crash. This should result in more modules from the CEC list to be inlcuded in the SAM module library. (`SSC 1245 <https://github.com/NatLabRockies/ssc/pull/1245>`__)

Fix implementation of NOCT cell temperature model. (`SSC 1233 <https://github.com/NatLabRockies/ssc/pull/1233>`__)

Improve error messages for weather files with invalid data for SAM and PySAM. (`SSC 1219 <https://github.com/NatLabRockies/ssc/pull/1219>`__)

Fix snow model for bifacial modules so that rear-side irradiance is not affected by snow cover. (`SSC 1214 <https://github.com/NatLabRockies/ssc/pull/1214>`__)

PVWatts (SAM 2024.12.12)
------------------------

Change variable group name for 'losses' input variable from "Other DC losses" to "DC system losses". (`SSC 1229 <https://github.com/NatLabRockies/ssc/pull/1229>`__)

Improve solar position algorithm to speed up PVWatts and Detailed PV simulation run times. (`SSC 1212 <https://github.com/NatLabRockies/ssc/pull/1212>`__)

Electric Battery Storage (SAM 2024.12.12)
-----------------------------------------

Modify battery dispatch to consider grid limits. (`SAM 1883 <https://github.com/NatLabRockies/SAM/pull/1883>`__, `SSC 1224 <https://github.com/NatLabRockies/ssc/pull/1224>`__)

Add battery-specific availability loss inputs. (`SAM 1902 <https://github.com/NatLabRockies/SAM/pull/1902>`__, `SSC 1241 <https://github.com/NatLabRockies/ssc/pull/1241>`__, `SSC 1234 <https://github.com/NatLabRockies/ssc/pull/1234>`__)

Add new Standalone Battery input to define simulation time step rather than basing it on other inputs. (`SAM 1887 <https://github.com/NatLabRockies/SAM/pull/1887>`__, `SSC 1227 <https://github.com/NatLabRockies/ssc/pull/1227>`__)

For behind-the-meter (BTM) batteries, Add metrics and graphs to Summary tab on Results page, and default graph showing battery performance metrics on Time Series tab. (`SAM 1884 <https://github.com/NatLabRockies/SAM/pull/1884>`__, `SAM 1865 <https://github.com/NatLabRockies/SAM/pull/1865>`__, `SSC 1215 <https://github.com/NatLabRockies/ssc/pull/1215>`__)

Change battery life model to base degraded battery capacity on sum of calendar and cycle degradation rather than the minimum of the two. (`SSC 1239 <https://github.com/NatLabRockies/ssc/pull/1239>`__)

Update and fix REopt API calls for battery size and dispatch optimization. (`SAM 1904 <https://github.com/NatLabRockies/SAM/pull/1904>`__, `SSC 1242 <https://github.com/NatLabRockies/ssc/pull/1242>`__, `SSC 1203 <https://github.com/NatLabRockies/ssc/pull/1203>`__)

Improve efficiency of internal handling of battery parameter variables to decrease simulation run time, particularly for PV smoothing dispatch option. (`SSC 1251 <https://github.com/NatLabRockies/ssc/pull/1251>`__)

Improve determination of price signal forecast for Merchant Plant configurations with automated dispatch: Calculate price signal in each time step as capacity-weighted sum of enabled energy and ancillary service prices instead of simple sum. (`SSC 1235 <https://github.com/NatLabRockies/ssc/pull/1235>`__)

Hybrid Systems (SAM 2024.12.12)
-------------------------------

Add input pages for thermal loads and thermal rates to hybrid configurations that include fuel cells. (`SAM 1903 <https://github.com/NatLabRockies/SAM/pull/1903>`__, `SSC 1244 <https://github.com/NatLabRockies/ssc/pull/1244>`__)

Include battery discharge power in power profile used to calculate project costs and benefits. (`SSC 1256 <https://github.com/NatLabRockies/ssc/issues/1256>`__)

Fix issue with PV-based O&M costs that scaled costs specified in $/kWac by PV DC capacity instead of PV AC capacity. (`)SSC 1260 <https://github.com/NatLabRockies/ssc/pull/1260>`__)

Use consistent default time-of-delivery (TOD) PPA price factors for all for front-of-meter (FOM) hybrid configurations. (`SAM 1971 <https://github.com/NatLabRockies/SAM/pull/1971>`__)

Remove "Hybrid" from performance model names in configuration window when creating or adding new cases. (`SAM 1875 <https://github.com/NatLabRockies/SAM/pull/1875>`__)

Concentrating Solar Power (CSP) (SAM 2024.12.12)
------------------------------------------------

For parabolic trough and linear Fresnel configurations, move input variables for receiver startup dely time and energy fraction from Solar Field page to System Control page. These inputs are for the dispatch optimization algorithm, not the performance simulation. (They are inputs to the performance model for power tower configurations.) (`SAM 1961 <https://github.com/NatLabRockies/SAM/pull/1961>`__, `SSC 1261 <https://github.com/NatLabRockies/ssc/pull/1261>`__)

New options for user-defined power cycle (UDPC) interpolation at maximum cycle output. (`SAM 1935 <https://github.com/NatLabRockies/SAM/pull/1935>`__, `SSC 1253 <https://github.com/NatLabRockies/ssc/pull/1253>`__)

Fix parabolic trough and linear Fresnel dispatch issue that resulted in infeasible results. (`SSC 1213 <https://github.com/NatLabRockies/ssc/pull/1213>`__)

New packed bed thermocline and pressurized water cylinder with piston separator TES options for the Physical Trough CSP and IPH performance models. (`SAM 1900 <https://github.com/NatLabRockies/SAM/pull/1900>`__, `SSC 1238 <https://github.com/NatLabRockies/ssc/pull/1238>`__)

Change default constant availability loss for molten salt power tower configurations from 0 to 5%. (`SAM 1899 <https://github.com/NatLabRockies/SAM/pull/1899>`__)

Industrial Process Heat (IPH) (SAM 2024.12.12)
----------------------------------------------

Add Single Owner and Commercial financial model option for IPH performance models. For older versions of SAM, only the LCOH Calculator (fixed-charge rate method of calculating levelized cost of heat) financial model was available. This major new feature involves adding new cash flow cost and benefit streams based on heat generated by the system. (`SAM 1900 <https://github.com/NatLabRockies/SAM/pull/1900>`__, `SSC 1238 <https://github.com/NatLabRockies/ssc/pull/1238>`__)

Add the Solar Dynamics SunBeam collector to the Physical Trough collector library. (`SAM 1900 <https://github.com/NatLabRockies/SAM/pull/1900>`__)

Disable Send-to-Excel with Equations (Windows only) for IPH configurations with Single Owner or Commercial financial model until workbooks are updated. (`SAM 1954 <https://github.com/NatLabRockies/SAM/pull/1954>`__)

Fix IPH molten salt linear Fresnel (MSLF) calculation of 'aux_design' and 'bop_design' to be consistent with CSP MSLF model. (`SSC 1254 <https://github.com/NatLabRockies/ssc/pull/1254>`__)

Marine Energy (SAM 2024.12.12)
------------------------------

Update PDF report template to support Wave and Tidal configurations with Single Owner and LCOE Calculator financial models, and new energy converter and resource options. (`SAM 1947 <https://github.com/NatLabRockies/SAM/pull/1947>`__)

Add time series availability losses to Wave Energy and Tidal Energy models. (`SAM 1885 <https://github.com/NatLabRockies/SAM/pull/1885>`__, `SSC 1225 <https://github.com/NatLabRockies/ssc/pull/1225>`__)

Add capacity factor input to tidal energy turbine design page. (`SAM 1872 <https://github.com/NatLabRockies/SAM/pull/1872>`__, `SSC 1218 <https://github.com/NatLabRockies/ssc/pull/1218>`__, `SSC 1181 <https://github.com/NatLabRockies/ssc/pull/1181>`__)

Update marine energy device cost curves. (`SSC 1249 <https://github.com/NatLabRockies/ssc/pull/1249>`__)

Update equations to improve use of marine energy models with PySAM. (`SSC 1217 <https://github.com/NatLabRockies/ssc/pull/1217>`__)

Geothermal Power (SAM 2024.12.12)
---------------------------------

Update cost scenario data. (`SAM 1931 <https://github.com/NatLabRockies/SAM/pull/1931>`__)

Add new option to turn off reservoir replacements. (`SAM 1881 <https://github.com/NatLabRockies/SAM/pull/1881>`__, `SSC 1222 <https://github.com/NatLabRockies/ssc/pull/1222>`__)

Add a button that updates O&M costs. (`SAM 1858 <https://github.com/NatLabRockies/SAM/pull/1858>`__, `SSC 1210 <https://github.com/NatLabRockies/ssc/pull/1210>`__)

Remove unused features EGS custom plant temperature, check for production pumping cost in flash plants, and user-interface text revisions. (`SAM 1857 <https://github.com/NatLabRockies/SAM/pull/1857>`__)

Improve production well calculations for brine efficiency and specific heat. (`SSC 1209 <https://github.com/NatLabRockies/ssc/pull/1209>`__)

Wind Power (SAM 2024.12.12)
---------------------------

Fix LandBOSSE call to run Python from SAM user interface. (`SAM 1939 <https://github.com/NatLabRockies/SAM/pull/1939>`__)

Add wind weather file reader check for missing data to improve error message for invalid wind resource files. (`SSC 1221 <https://github.com/NatLabRockies/ssc/pull/1221>`__)

New inputs to SSC (accessible only via `SAM Software Development Kit (SDK) <https://sam.nrel.gov/software-development-kit-sdk>`__ for wake model, access to internal Park wake model inputs, and new "icing persistence" input. These features are not available in the SAM desktop application. (`SSC 1186 <https://github.com/NatLabRockies/ssc/pull/1186>`__)

Custom Generation Profile (SAM 2024.12.12)
------------------------------------------

Rename "Generic System" model to "Custom Generation Profile" to be clearer that model can use a time series generation profile from measured data or from a different model as input. (`SAM 1890 <https://github.com/NatLabRockies/SAM/pull/1890>`__, `SSC 1228 <https://github.com/NatLabRockies/ssc/pull/1228>`__)

.. _2023-12-17-r2:

SAM 2023.12.17 r2, SSC 292: August 23, 2024
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the second maintenance update to :ref:`SAM 2023.12.17 <2023-12-17>`.

- Detailed PV and PVWatts

  - Set default PV uncertainty input distributions for factors that are represented by Detailed PV/PVWatts performance model, and add checkboxes to PV uncertainty categories to allow for enabling and disabling them. (`SAM 1812 <https://github.com/NatLabRockies/SAM/pull/1812>`__, `SAM 1753 <https://github.com/NatLabRockies/SAM/pull/1753>`__)
  - Add checkboxes to PV uncertainty page to allow for enabling and disabling uncertainty categories. (`SAM 1753 <https://github.com/NatLabRockies/SAM/pull/1753>`__)
  - Fix problems with PV PDF report. (`SAM 1789 <https://github.com/NatLabRockies/SAM/pull/1789>`__)
  - Change diagrams of tracking and orientation on Sytem Design page for Detailed PV and PVWatts models, and revise Help descriptions of tilt and azimuth inputs and labels of plane-of-array (POA) irradiance output variables. (`SAM 1791 <https://github.com/NatLabRockies/SAM/pull/1791>`__, `SSC 1153 <https://github.com/NatLabRockies/ssc/pull/1153>`__)
  - Improve documentation of IEC 61858 temperature, optical, and spectral models for PV. (`SAM 1795 <https://github.com/NatLabRockies/SAM/pull/1795>`__, `SSC 1175 <https://github.com/NatLabRockies/ssc/pull/1175>`__)
  - Fix internal issue with initializing ground clearance height for IEC 61853 module model. (`SSC 1194 <https://github.com/NatLabRockies/ssc/pull/1194>`__)

- Battery Storage

  - Fix default battery properties for Li-NMC batteries for some configurations with batteires. (`SAM 1818 <https://github.com/NatLabRockies/SAM/pull/1818>`__)
  - Improve battery lifetime model to allow for reversible thermal degradation. (`SSC 1164 <https://github.com/NatLabRockies/ssc/pull/1164>`__)
  - Fix issues with REopt API call for battery size and dispatch optimization. (`SSC 1187 <https://github.com/NatLabRockies/ssc/pull/1187>`__)
  - Display a message on input page when battery is allowed to charge from grid an a production tax credit (PTC) is enabled. (`SAM 1792 <https://github.com/NatLabRockies/SAM/pull/1792>`__)
  - Improve results of battery storage self-consumption dispatch option to more accurately determine when 100% of load is met by the system. (`SSC 1182 <https://github.com/NatLabRockies/ssc/pull/1182>`__)
  - Fix PDF report for the self-consumption battery dispatch option. (`SAM 1750 <https://github.com/NatLabRockies/SAM/pull/1750>`__)
  - Fix year index in TOU demand charge calculation for electricity bill calculations. (`SSC 1152 <https://github.com/NatLabRockies/ssc/pull/1152>`__)

- Financial Models

  - Fix tax and depreciation formulas in Send-to-Excel workbooks. (`SAM 1841 <https://github.com/NatLabRockies/SAM/pull/1841>`__)
  - Fix electricity bill calculation for net billing with tiered energy rates. (`SSC 1168 <https://github.com/NatLabRockies/ssc/pull/1168>`__)
  - Report basis for investment tax credit (ITC) and depreciation in outputs to improve transparency of incentive calculations. (`SAM 1808 <https://github.com/NatLabRockies/SAM/pull/1808>`__, `SSC 1178 <https://github.com/NatLabRockies/ssc/pull/1178>`__)
  - Fix formula in Excel Workbook for Third Party Host / Developer financial model that calculates federal depreciation. (`SAM 1793 <https://github.com/NatLabRockies/SAM/pull/1793>`__)
  - Fix formulas in Excel Workbook for Community Solar financial model that calculates subscriber share of generation. (`SAM 1744 <https://github.com/NatLabRockies/SAM/pull/1744>`__)
  - Revise description of Indifference Point macro for Third Party Host / Developer model. (`SAM 1788 <https://github.com/NatLabRockies/SAM/pull/1788>`__)

- Update weather file downloads from the Advanced NSRDB Download window to make all attributes and time resolutions available from latest PSM V3.2.2 endpoints. (`SAM 1746 <https://github.com/NatLabRockies/SAM/pull/1746>`__)
- Fix stochastic and PV uncertainty simulations on macOS. (`SAM 1758 <https://github.com/NatLabRockies/SAM/pull/1758>`__, `SAM 1782 <https://github.com/NatLabRockies/SAM/pull/1782>`__)
- Revise Marine Energy Help topics mentioning DOE Reference Model project. (`SAM 1790 <https://github.com/NatLabRockies/SAM/pull/1790>`__)
- Revise description of Wind environmental loss on input page and in Help. (`SAM 1770 <https://github.com/NatLabRockies/SAM/pull/1770>`__)
- Fix custom HTF for Molten Salt Power Tower configurations. (`SAM 1817 <https://github.com/NatLabRockies/SAM/pull/1817>`__)
- Fix problem reading time series loss data (converting from old 'adjust:periods' to new 'adjust_periods' variable names) when opening .sam files from older versions. (`SAM 1749 <https://github.com/NatLabRockies/SAM/pull/1749>`__)
- Fix application of time series losses ('adjust' variables) for non-annual simulations from SAM SDK / PySAM. (`SSC 1113 <https://github.com/NatLabRockies/ssc/pull/1113>`__, `SAM 1826 <https://github.com/NatLabRockies/SAM/pull/1826>`__)
- Make geothermal cost curves available in SSC (`SSC 1191 <https://github.com/NatLabRockies/ssc/pull/1191>`__)
- Fix issues with geothermal cost inputs. (`SAM 1823 <https://github.com/NatLabRockies/SAM/pull/1823>`__, `SSC 1189 <https://github.com/NatLabRockies/ssc/pull/1189>`__)
- Modify marine energy tidal energy converter "Design tidal converter" power curve calculation method. (`SAM 1807 <https://github.com/NatLabRockies/SAM/pull/1807>`__)

.. _2023-12-17-r1:

SAM 2023.12.17 r1, SSC 290: March 4, 2024
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the first maintenance update to :ref:`SAM 2023.12.17 <2023-12-17>`.

- Solar Resource

  - Support NSRDB weather file downloads from the Solar Resource page for locations outside the United States. (`SAM 1720 <https://github.com/NatLabRockies/SAM/pull/1720>`__, `SSC 1140 <https://github.com/NatLabRockies/ssc/pull/1140>`__). `Additional work is planned <https://github.com/NatLabRockies/SAM/issues/1534>`__ for the Advanced NSRDB Download window for the second update to :ref:`SAM 2023.12.17 <2023-12-17>`.
  - Fix problem using latitude and longitude to download weather files from the Solar Resource page. (`SAM 1692 <https://github.com/NatLabRockies/SAM/pull/1692>`__)

- Detailed PV and PVWatts

  - Fix PV land area calculations. (`SAM 1721 <https://github.com/NatLabRockies/SAM/pull/1721>`__, `SAM 1697 <https://github.com/NatLabRockies/SAM/pull/1697>`__)
  - Fix PV time series shading data import from 3D shade calculator and overlapping inputs in Edit Shading window for PVWatts. (`SAM 1712 <https://github.com/NatLabRockies/SAM/pull/1712>`__)
  - Fix Detailed PV non-linear self shading for vertical arrays to avoid negative POA irradiance values. (`SSC 1127 <https://github.com/NatLabRockies/ssc/pull/1127>`__)
  - Fix issue with Detailed PV subhourly clipping loss when weather file has no DNI data. (`SSC 1126 <https://github.com/NatLabRockies/ssc/pull/1126>`__)
  - Fix reporting of ground-reflected rear irradiance gains in loss diagram for Detailed PV model with bifacial modules. (`SSC 1125 <https://github.com/NatLabRockies/ssc/pull/1125>`__)
  - Add snow slide coefficient to outputs for Detailed PV with snow model enabled. (`SSC 1128 <https://github.com/NatLabRockies/ssc/pull/1128>`__)
  - Clean up PV subhourly clipping loss correction in user interface. (`SAM 1689 <https://github.com/NatLabRockies/SAM/pull/1689>`__)
  - Fix diagrams on Detailed PV Shading and Layout page to show both portrait and landscape instead of duplicating portrait. (`SAM 1696 <https://github.com/NatLabRockies/SAM/pull/1696>`__)

- Concentrating Solar Power (CSP)

  - Fix parabolic trough loop configuration on SCA (Collectors) page. (`SAM 1709 <https://github.com/NatLabRockies/SAM/pull/1709>`__)
  - Fix problems with parabolic trough dispatch optimization and loop configuration that caused a simulation error. (`SAM 1674 <https://github.com/NatLabRockies/SAM/issues/1674>`__, `SSC 1121 <https://github.com/NatLabRockies/ssc/pull/1121>`__)

- Battery Storage

  - Improve REopt API calls for battery size and dispatch optimization to better report status and errors from API. (`SAM 1714 <https://github.com/NatLabRockies/SAM/pull/1714>`__)
  - Fix simulation error caused by battery environment temperature input as time series array. (`SAM 1694 <https://github.com/NatLabRockies/SAM/pull/1694>`__, `SSC 1138 <https://github.com/NatLabRockies/ssc/pull/1138>`__)
  - Fix issue with behind-the-meter battery with retail rate dispatch that caused a simulation error. (`SSC 1135 <https://github.com/NatLabRockies/ssc/pull/1135>`__)
  - Fix default battery life inputs so correct table is shown for the default Li-ion LFP/Graphite battery type. (`SAM 1715 <https://github.com/NatLabRockies/SAM/pull/1715>`__)
  - Fix problem with battery discharging to grid for manual dispatch option. (`SSC 1115 <https://github.com/NatLabRockies/ssc/pull/1115>`__)
  - Fix problem with battery self consumption dispatch option with grid outage. (`SSC 1114 <https://github.com/NatLabRockies/ssc/pull/1114>`__)

- Fix problem with losses specifed as "time series losses with custom time periods" that caused a simulation error. (`SAM 1698 <https://github.com/NatLabRockies/SAM/pull/1698>`__)
- Improve reporting of time series data for subhourly Detailed PV and PV Battery simulations so that "hourly" and "hourly lifetime" values are consistent. (`SAM 1684 <https://github.com/NatLabRockies/SAM/issues/1684>`__)
- Fix problem with loading weather files with time series wave resource data for marine energy wave model. (`SAM 1701 <https://github.com/NatLabRockies/SAM/pull/1701>`__, `SSC 1130 <https://github.com/NatLabRockies/ssc/pull/1130>`__)
- Improve debt sizing calculation for front-of-meter financial models with sculpted debt (DSCR as input) and when debt size limit is enabled. (`SSC 1136 <https://github.com/NatLabRockies/ssc/pull/1136>`__)
- Fix state of Fuel Cell dispatch page when opening .sam files from older versions of SAM. (`SAM 1707 <https://github.com/NatLabRockies/SAM/pull/1707>`__)
- Fix Save and Load buttons throughout user interface. (`SAM 1695 <https://github.com/NatLabRockies/SAM/pull/1695>`__)
- Improve reporting of DC loss factors in PDF report for Detailed PV and PV Battery configurations. (`SAM 1690 <https://github.com/NatLabRockies/SAM/pull/1690>`__)
- Fix broken link to SAM Forum from Help window. (`SAM 1716 <https://github.com/NatLabRockies/SAM/pull/1716>`__)

.. _2023-12-17:

SAM 2023.12.17, SSC 288: December 17, 2023
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version introduces a set of new hybrid power system models and a new molten salt linear Fresnel models for electric power generation and industrial process heat (IPH) applications.

For a complete list of code revisions associated with this version, see the `SAM <https://github.com/NatLabRockies/SAM/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2023+Release%22+is%3Aclosed>`__ and `SSC <https://github.com/NatLabRockies/ssc/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2023+Release%22+is%3Aclosed>`__ GitHub repositories. Highlights include:

- New SAM logo and icon.
- New hybrid power models with different combinations of the photovoltaic (PVWatts and Detailed PV), Wind Power, Fuel Cell, and Generic System models with battery storage for the Single Owner and Third Party Host / Developer financial models.
- New industrial process heat (IPH) models for molten salt power tower and molten salt linear Fresnel systems.
- Improve molten salt linear Fresnel concentrating solar power (CSP) model.
- Update REopt for PV-battery optimization to REopt API V3.
- New self-consumption dispatch option for behind-the-meter batteries.
- Geothermal model updates.
- Marine Energy model updates.
- Update defaults.
- Update photovoltaic module and inverter libraries.
- Revise and update Help.

We are working to fix known `SAM issues <https://github.com/NatLabRockies/SAM/issues?q=is%3Aopen+is%3Aissue+milestone%3A%22SAM+Fall+2023+Release%22>`__ and `SSC issues <https://github.com/NatLabRockies/ssc/issues?q=is%3Aopen+is%3Aissue+milestone%3A%22SAM+Fall+2023+Release%22>`__ for an update to this version in early 2024. Please `let us know <mailto:sam.support@nrel.gov>`__ if you find any other problems.

Hybrid Power Systems
--------------------

This major new feature adds configurations for hybrid power systems. (`SAM 1448 <https://github.com/NatLabRockies/SAM/pull/1448>`__ and `multiple subsequent pull requests <https://github.com/NatLabRockies/SAM/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2023+Release%22+label%3A%22dev+bug%22+is%3Aclosed>`__, `SSC 1051 <https://github.com/NatLabRockies/ssc/pull/1051>`__)

Hybrid power systems combine photovoltaic (PVWatts or Detailed PV), Wind Power, Fuel Cell, and Generic System models for power generation with the Standalone Battery model for electricity storage. Battery dispatch responds to the sum of power from the PV, wind, fuel cell and/or generic systems in each time step. The hybrid system performance models are available with either the Single Owner for front-of-meter applications or the Third Party Host / Developer financial model for behind-the-meter applications. As is the case for all of SAM's models, SAM assumes the hybrid system is connected to the grid. The new hybrid configurations are available for the following combinations of performance models:

- PVWatts, Wind Power, Battery Storage
- PVWatts, Wind Power, Fuel Cell, Battery Storage
- Detailed PV, Wind Power, Battery Storage
- Generic System, PVWatts, Wind Power, Fuel Cell, Battery Storage

General (2023.12.17)
--------------------

- New System Advisor Model logo and icon. Icon is implemented for Windows and Mac, not Linux. (`SAM 1514 <https://github.com/NatLabRockies/SAM/pull/1514>`__)
- Update National Solar Radiation Database (NSRDB) downloads from Location and Resource input page to NSRDB PSM V3.2.2 to include latest data from 2022 and update sample LK script. (`SAM 1559 <https://github.com/NatLabRockies/SAM/pull/1559>`__, `SAM 1499 <https://github.com/NatLabRockies/SAM/pull/1499>`__)
- Update defaults to align with `2023 NREL Annual Technology Baseline (ATB) <https://atb.nrel.gov/electricity/2023/index>`__ and to result in positive net present value (NPV) where possible. (`SAM 1581 <https://github.com/NatLabRockies/SAM/pull/1581>`__)
- Fix layout issues for Mac and Linux versions. (`SAM 1580 <https://github.com/NatLabRockies/SAM/pull/1580>`__)
- Change format of "adjust" variable used for availability and other time-varying losses from table to separate variables for "constant", "time series" and "periods." This internal change only affects LK scripts and code for the Software Development Kit (SDK) and PySAM. (`SAM 1468 <https://github.com/NatLabRockies/SAM/pull/1468>`__)
- Fix issue with time series availability losses. (`SSC 1093 <https://github.com/NatLabRockies/ssc/pull/1093>`__)

Financial Models (2023.12.17)
-----------------------------

- Fix investment-based incentive (IBI) and capacity-based incentive (CBI) double-counting in cash flow (`SAM 1445 <https://github.com/NatLabRockies/SAM/pull/1445>`__, `SSC 1048 <https://github.com/NatLabRockies/ssc/pull/1048>`__)
- Update Send-to-Excel with Equations (Windows only) for battery and fuel cell / residential and commercial configurations to properly account for battery and fuel cell operating costs. (`SAM 1545 <https://github.com/NatLabRockies/SAM/pull/1545>`__)
- Update Send-to-Excel with Equations (Windows only) for front-of-meter (FOM) financial models to calculate production based incentive (PBI), production-based O&M costs, and production tax credit (PTC) from correct annual energy value. (`SAM 1501 <https://github.com/NatLabRockies/SAM/pull/1501>`__, `SSC 1066 <https://github.com/NatLabRockies/ssc/pull/1066>`__)
- Add user interface warning messages for systems with battery storage and a production tax credit (PTC) when battery is allowed to charge from the grid. (`SAM 1542 <https://github.com/NatLabRockies/SAM/pull/1542>`__)
- Fix electricity to/from grid output for battery configurations with grid outages. (`SSC 1097 <https://github.com/NatLabRockies/ssc/pull/1097>`__)
- Report battery capacity-based O&M expense in cash flow for all financial models. (`SSC 1076 <https://github.com/NatLabRockies/ssc/pull/1076>`__)
- Remove total production tax credit (PTC) from cash flow results. State and federal PTC are applied separately. (`SSC 1072 <https://github.com/NatLabRockies/ssc/pull/1072>`__)
- Fix monthly energy values for grid curtailment. (`SSC 1068 <https://github.com/NatLabRockies/ssc/pull/1068>`__)
- Rename internal cash flow variables from "net" to "sales" to make it clear these are for energy to grid outputs. (`SSC 1060 <https://github.com/NatLabRockies/ssc/pull/1060>`__)

Detailed Photovoltaic (2023.12.17)
----------------------------------

- New subhourly clipping correction for AC loss calculation. (`SAM 1447 <https://github.com/NatLabRockies/SAM/pull/1447>`__, `SSC 1077 <https://github.com/NatLabRockies/ssc/pull/1077>`__, `SSC 1050 <https://github.com/NatLabRockies/ssc/pull/1050>`__)
- Update module and inverter libraries from 11/17/2023 CEC workbooks. (`SAM 1607 <https://github.com/NatLabRockies/SAM/pull/1607>`__, `SAM 1606 <https://github.com/NatLabRockies/SAM/pull/1606>`__)
- Change default inverters based on market share. (`SAM 1607 <https://github.com/NatLabRockies/SAM/pull/1607>`__)
- Make scripts and data used to generate module and inverter libraries public. (`SAM 1440 <https://github.com/NatLabRockies/SAM/pull/1440>`__)
- Fix issue with availability and shading losses shown on PDF report. (`SAM 1579 <https://github.com/NatLabRockies/SAM/pull/1579>`__)
- Add checks and warnings for bifacial ground clearance height input values that result in modules being too close to the ground. (`SAM 1544 <https://github.com/NatLabRockies/SAM/pull/1544>`__, `SSC 1095 <https://github.com/NatLabRockies/ssc/pull/1095>`__)
- Move module dimension inputs from Shading and Layout page to Module page. (`SAM 1519 <https://github.com/NatLabRockies/SAM/pull/1519>`__)
- Change default inverter temperature derating curve maximum voltage to 1500 VDC so it works with full range of inverters in the library. (`SAM 1460 <https://github.com/NatLabRockies/SAM/pull/1460>`__)
- Improve PVsyst conversion macro to better handle PVsyst meteo files. (`SAM 1430 <https://github.com/NatLabRockies/SAM/pull/1430>`__)
- Prevent number of strings in parallel value of zero to avoid simulation crash. (`SSC 1087 <https://github.com/NatLabRockies/ssc/pull/1087>`__)
- Improve error messages when cell temperature heat transfer method is missing data. (`SSC 1086 <https://github.com/NatLabRockies/ssc/pull/1086>`__)
- New input for time series one-axis tracker rotation axis. This is implemented for SSC only so available in the SAM Software Development Kit (SDK) including PySAM, but not available in the SAM user interface. (`SSC 1071 <https://github.com/NatLabRockies/ssc/pull/1071>`__)

PVWatts (2023.12.17)
--------------------

- Fix issue with Edit Shading Data window for advanced "Shading by Nearby Objects". (`SAM 1593 <https://github.com/NatLabRockies/SAM/pull/1593>`__)

Battery Storage (2023.12.17)
----------------------------

- Change default battery type from Li-ion LFP to Li-ion NMC. (`SAM 1581 <https://github.com/NatLabRockies/SAM/pull/1581>`__)
- New behind-the-meter self-consumption battery dispatch option that minimizes use of grid power (`SAM 1446 <https://github.com/NatLabRockies/SAM/pull/1446>`__, `SSC 1057 <https://github.com/NatLabRockies/ssc/pull/1057>`__, `SSC 1049 <https://github.com/NatLabRockies/ssc/pull/1049>`__)
- Update REopt API call to Version 3, pass grid outage data to API, and fix conversion efficiency values. (`SAM 1527 <https://github.com/NatLabRockies/SAM/pull/1527>`__, `SSC 1078 <https://github.com/NatLabRockies/ssc/pull/1078>`__)
- Rename "price signals dispatch" to "retail rate dispatch" for behind-the-meter battery dispatch options. (`SAM 1495 <https://github.com/NatLabRockies/SAM/pull/1495>`__, `SSC 1064 <https://github.com/NatLabRockies/ssc/pull/1064>`__)
- Fix problem with automatic battery sizing when desired size is specified in AC units. (`SAM 1469 <https://github.com/NatLabRockies/SAM/pull/1469>`__)
- Implement internal resistance for batteries using the voltage table, including lead acid battery types, also change default lead acid inernal resistance to a more realistic value. (`SAM 1493 <https://github.com/NatLabRockies/SAM/pull/1493>`__, `SSC 1016 <https://github.com/NatLabRockies/ssc/pull/1016>`__)
- Remove "Calculate Load Data" option from Electric Load page for Standalone Battery / Residential configurations because it requires solar resource data that is not available. (`SAM 1467 <https://github.com/NatLabRockies/SAM/pull/1467>`__)
- Set default values of all availability loss inputs to zero for battery configurations. (`SAM 1463 <https://github.com/NatLabRockies/SAM/pull/1463>`__)
- Show rear irradiance values in loss diagram for PV-battery configurations. (`SAM 1422 <https://github.com/NatLabRockies/SAM/pull/1422>`__)
- Fix issue for PV-smoothing dispatch that caused discrepancy between PV-smoothing and battery state of charge values. (`SSC 1085 <https://github.com/NatLabRockies/ssc/pull/1085>`__)
- Fix battery not discharging for peak shaving dispatch for small batteries. (`SSC 1208 <https://github.com/NatLabRockies/ssc/pull/1208>`__)
- Fix battery discharging in excess of critical load when AC losses are present. (`SSC 1061 <https://github.com/NatLabRockies/ssc/pull/1061>`__)

Concentrating Solar Power (2023.12.17)
--------------------------------------

- Update molten salt linear Fresnel CSP model to use plant controller and numerical solver used by power tower and trough models. This framework improves mass and energy balances, especially during timeperiods when the field, TES, and cycle are all operating. This implementation also enables dispatch optimization. Note that the fossil backup option was removed as part of this work. The default case in earlier releases used fossil backup to provide freeze protection, and this release switches to using electricity that is deducted from the total plant output. As such, the annual energy in the default case decreases by around 12%. When comparing to the prior release without fossil freeze protection, the new release has slightly better annual energy due to improved thermal transient modeling that reduces freeze protection requirements.
- New industrial process heat (IPH) models for molten salt power tower and molten salt linear Fresnel systems.
- Fix bug in calculation of thermal losses from brackets in the evacuated tube receiver model used in the Physical trough and Fresnel models. The annual energy of the default Physical trough CSP system decreased by around 0.5% and the annual energy of the default Fresnel CSP system decreases by around 2%.
- Improved the calculations that size the Physical trough and Fresnel fields to achieve to a target solar multiple. The new implementation includes all design-point optical and thermal losses. For the default cases, this change increases the solar field size by 1-2%.
- Improved levelized cost of heat (LCOH) model includes component cost breakdowns
- Fix molten salt power tower cavity receiver estimated net output at design calculation. (`SSC 1067 <https://github.com/NatLabRockies/ssc/pull/1067>`__)

Wind Power (2023.12.17)
-----------------------

- Add system availability loss to wind power model. `SAM 1612 <https://github.com/NatLabRockies/SAM/pull/1612>`__
- Fix WIND Toolkit downloads to correctly download data at different measurement heights. (`SAM 1483 <https://github.com/NatLabRockies/SAM/pull/1483>`__)

Marine Energy (2023.12.17)
--------------------------

- New array cable voltage and array cost scaling calculations. (`SAM 1423 <https://github.com/NatLabRockies/SAM/pull/1423>`__, `SAM 1350 <https://github.com/NatLabRockies/SAM/pull/1350>`__, `SSC 1038 <https://github.com/NatLabRockies/ssc/pull/1038>`__, `SSC 990 <https://github.com/NatLabRockies/ssc/pull/990>`__)
- New option for custom tidal turbine design. (`SAM 1423 <https://github.com/NatLabRockies/SAM/pull/1423>`__)
- New option for Tidal resource time series data with new example resource files. (`SAM 1510 <https://github.com/NatLabRockies/SAM/pull/1510>`__, `SSC 1069 <https://github.com/NatLabRockies/ssc/pull/1069>`__)
- Fix user interface layout issues for Mac, and issues with showing and hiding inputs on cost pages cost based on selection of options. (`SAM 1591 <https://github.com/NatLabRockies/SAM/pull/1591>`__)
- Update Wave resource libraries and example files. (`SAM 1591 <https://github.com/NatLabRockies/SAM/pull/1591>`__, `SSC 1095 <https://github.com/NatLabRockies/ssc/pull/1095>`__)
- Fix issue with tidal resource data shown on PDF report. (`SAM 1579 <https://github.com/NatLabRockies/SAM/pull/1579>`__)

Solar Water Heating
-------------------

- Limit tank cold node temperature to realistic range of values. (`SSC 1011 <https://github.com/NatLabRockies/ssc/pull/1011>`__)

Geothermal Power (2023.12.17)
-----------------------------

- Fix reservoir pressure change units. (`SAM 1591 <https://github.com/NatLabRockies/SAM/pull/1591>`__, `SSC 1095 <https://github.com/NatLabRockies/ssc/pull/1095>`__)
- Add inputs for more precise well count calculations, new tooltips for input variable descriptions, and option to read wet bulb temperature from weather file. (`SAM 1524 <https://github.com/NatLabRockies/SAM/pull/1524>`__, `SSC 1075 <https://github.com/NatLabRockies/ssc/pull/1075>`__)
- Disable Default Type options on Geothermal Resource page for configurations with LCOE Calculator or No Financial that have no analysis period defined to avoid user interface "callback" error. (`SAM 1517 <https://github.com/NatLabRockies/SAM/pull/1517>`__, `SAM 1488 <https://github.com/NatLabRockies/SAM/pull/1488>`__)

Fuel Cell (2023.12.17)
----------------------

- Create new input page for fuel cell dispatch options that is separate from battery dispatch, and remove options not appropriate for fuel cell. (`SAM 1492 <https://github.com/NatLabRockies/SAM/pull/1492>`__)
- Add monthly energy output. (`SSC 1094 <https://github.com/NatLabRockies/ssc/pull/1094>`__)

.. _2022-11-21-r3:

SAM 2022.11.21 r3, SSC 280: June 3, 2023
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the third maintenance update to :ref:`SAM 2022.11.21 <2022-11-21>`.

- Fix Electric Load page callback error. (`SAM 1412 <https://github.com/NatLabRockies/SAM/pull/1412>`__)
- Help revisions. (`SAM 1413 <https://github.com/NatLabRockies/SAM/pull/1413>`__)

.. _2022-11-21-r2:

SAM 2022.11.21 r2, SSC 280: May 27, 2023
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the second maintenance update to :ref:`SAM 2022.11.21 <2022-11-21>`.

- Fix the Edit Losses window (LossAdjustment widget): Improve summary text that appears on input page to make it clearer how time series and custom period losses are applied, fix input modes, and fix widget for CSP solar field availability inputs. (`SAM 1408 <https://github.com/NatLabRockies/SAM/pull/1408>`__, `SSC 1024 <https://github.com/NatLabRockies/ssc/pull/1024>`__, `SSC 1028) <https://github.com/NatLabRockies/ssc/pull/1028>`__
- Fix marine energy PDF report rounding of numbers and add LCOE and FCR table. (`SAM 1399 <https://github.com/NatLabRockies/SAM/pull/1399>`__)
- Fix photovoltaic PDF report so monthly energy graphs display correctly for large systems. (`SAM 1394 <https://github.com/NatLabRockies/SAM/pull/1394>`__)
- Fix critical load array length problems. (`SAM 1397 <https://github.com/NatLabRockies/SAM/pull/1397>`__, `SSC 1021 <https://github.com/NatLabRockies/ssc/pull/1021>`__, `SAM 1390 <https://github.com/NatLabRockies/SAM/pull/1390>`__)
- Update WIND Toolkit download sample scripts. (`SAM 1396 <https://github.com/NatLabRockies/SAM/pull/1396>`__)
- Fix photovoltaic loss diagram to avoid overlapping text. (`SAM 1393 <https://github.com/NatLabRockies/SAM/pull/1393>`__)
- Fix photovoltaic CAPEX graphs on Installation Costs page. (`SAM 1386 <https://github.com/NatLabRockies/SAM/pull/1386>`__)
- Fix direct steam linear Fresnel callback error on System Design page. (`SAM 1382 <https://github.com/NatLabRockies/SAM/pull/1382>`__)
- Fix high-concentration photovoltaic callback error on Inverter page. (`SAM 1381 <https://github.com/NatLabRockies/SAM/pull/1381>`__)
- Fix geothermal cost calculations for automatic plant cost estimate. (`SAM 1379 <https://github.com/NatLabRockies/SAM/pull/1379>`__)
- Revise multiple weather file download instructions in page note. (`SAM 1377 <https://github.com/NatLabRockies/SAM/pull/1377>`__)
- Improve photovoltaic sizing message logic and text. (`SAM 1376 <https://github.com/NatLabRockies/SAM/pull/1376>`__)
- Update WIND Toolkit downloads to use wtk-download endpoint instead of wtk-srw-download to provide access to subhourly data. (`SAM 1374 <https://github.com/NatLabRockies/SAM/pull/1374>`__)
- Update hourly price downloads to Cambium 2022 and add an optional escalation rate. (`SAM 1365 <https://github.com/NatLabRockies/SAM/pull/1365>`__)
- Update battery price signals dispatch to use same approach as peak shaving so battery dispatch can respond to system power and load. (`SSC 1031 <https://github.com/NatLabRockies/ssc/pull/1031>`__)
- Update REopt battery sizing for standalone battery, includes updating PySAM tools for Utility Rate Database (URDB) to URDB Version 8. (`SSC 1022 <https://github.com/NatLabRockies/ssc/pull/1022>`__)
- Add check on battery voltage table for duplicate voltage values to prevent battery charge power exceeding available power from system. (`SSC 1020 <https://github.com/NatLabRockies/ssc/pull/1020>`__)
- Update weather file reader to avoid error about different number of columns in header rows for SAM CSV solar resource file format. (`SSC 1017 <https://github.com/NatLabRockies/ssc/pull/1017>`__)

.. _2022-11-21-r1:

SAM 2022.11.21 r1, SSC 279: February 23, 2023
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the first maintenance update to :ref:`SAM 2022.11.21 <2022-11-21>`.

- Add support to Wind Power model for wind resource file format compatible with WIND Toolkit API. The Wind model still reads wind resource files in SRW format (.srw file extension) for legacy wind resource files, but the `SAM CSV format for WIND <https://samrepo.nrelcloud.org/help/weather_format_csv_wind.htm>`__ is now SAM's default format for wind resource files. (`SSC 985 <https://github.com/NatLabRockies/ssc/pull/985>`__)
- Add Artic XCPC collector to Solar Water Heating collector library. (`SAM 1337 <https://github.com/NatLabRockies/SAM/pull/1337>`__)
- Fix Geothermal resource temperature input constraint on maximum value. (`SAM 1341 <https://github.com/NatLabRockies/SAM/pull/1341>`__, `SSC 986 <https://github.com/NatLabRockies/ssc/pull/986>`__)
- Fix issue for Geothermal resource temperature less than 150°C causing capacity factor greater than 1. (`SSC 989 <https://github.com/NatLabRockies/ssc/pull/989>`__)
- New Python sample script to download data from the OpenEI End Use Load Profiles database. (`SAM 1331 <https://github.com/NatLabRockies/SAM/pull/1331>`__)
- Convert UI forms to JSON. (`SAM 1338 <https://github.com/NatLabRockies/SAM/pull/1338>`__, `WEX 155 <https://github.com/NatLabRockies/wex/pull/155>`__)
- Fix descriptive text in Loss Adjustment widget. (`SAM 1355 <https://github.com/NatLabRockies/SAM/pull/1355>`__)
- Fix 'irrad_mode' input variable documentation. (`SSC 955 <https://github.com/NatLabRockies/ssc/pull/955>`__)
- Revise and update Help. (`SAM 1359 <https://github.com/NatLabRockies/SAM/pull/1359>`__)
- Battery Storage

  - Fix issue with Generic Battery price signal dispatch causing simulation crash. (`SSC 991 <https://github.com/NatLabRockies/ssc/pull/991>`__)
  - Fix missing Help topic for Stanadalone Battery installation cost page. (`SAM 1347 <https://github.com/NatLabRockies/SAM/pull/1347>`__)
  - Fix issue with PV - Battery configurations caused by specifying negative time series losses. (`SSC 980 <https://github.com/NatLabRockies/ssc/pull/980>`__)

- CSP, IPH and TES

  - New Pumped Thermal Energy Storage (PTES) design-point parameter calculator. (`SAM 1318 <https://github.com/NatLabRockies/SAM/pull/1318>`__, `SSC 965 <https://github.com/NatLabRockies/ssc/pull/965>`__)
  - Add methanol as cold TES option for Pumped Thermal Energy Storage (PTES). (`SAM 1307 <https://github.com/NatLabRockies/SAM/pull/1307>`__, `SSC 967 <https://github.com/NatLabRockies/ssc/pull/967>`__)
  - Fix Electric Thermal Energy Storage (ETES) issue loading input pages. (`SAM 1312 <https://github.com/NatLabRockies/SAM/pull/1312>`__, `SSC 961 <https://github.com/NatLabRockies/ssc/pull/961>`__)
  - Fix Physical Trough labeling of SCA and HCE in loop configuration widget and SCA and HCE input pages. (`SAM 1329 <https://github.com/NatLabRockies/SAM/pull/1329>`__)
  - Fix issue with Physical Trough at 600 MWt heat capacity simulation failing. (`SSC 982 <https://github.com/NatLabRockies/ssc/pull/982>`__)
  - Fix Molten Salt Power Tower optimization failure and erroneous calculated design point values of -789. (`SSC 976 <https://github.com/NatLabRockies/ssc/pull/976>`__)
  - Add ternery salt properties to HTF options for Molten Salt Power Tower. (`SAM 1321 <https://github.com/NatLabRockies/SAM/pull/1321>`__)
  - Fix Industrial Process Heat (IPH) Trough and Physical Trough models to allow use of 'solar_resource_data' input. (`SSC 975 <https://github.com/NatLabRockies/ssc/pull/975>`__)
  - Fix issue with name of system capacity variable in internal code. (`SSC 958 <https://github.com/NatLabRockies/ssc/pull/958>`__)

- Financial Models

  - Fix issue with NaN in financial model results for performance models that run single year simulations. (`SSC 993 <https://github.com/NatLabRockies/ssc/pull/993>`__)
  - Fix issue with display of lifetime electricity load output for performance models with singe-year simulations. (`SSC 963 <https://github.com/NatLabRockies/ssc/pull/963>`__)
  - Fix ITC for PPA Sale Leaseback to apply to all years. (`SSC 988 <https://github.com/NatLabRockies/ssc/pull/988>`__)
  - Update cash flow Excel Send-to-Excel with Equations to support multi-year ITC. (`SAM 1343 <https://github.com/NatLabRockies/SAM/pull/1343>`__)
  - Electricity rate energy charge supports up to 36 TOU periods. (`SAM 1357 <https://github.com/NatLabRockies/SAM/pull/1357>`__)
  - Add button to Electric Load page to run "Download Load Data" macro. (`SAM 1349 <https://github.com/NatLabRockies/SAM/pull/1349>`__)
  - Fix issue with grid outage code that cause simulation to stop without finishing. (`SSC 979 <https://github.com/NatLabRockies/ssc/pull/979>`__)
  - Improve output variable labels to make effect of grid interconnection clearer. (`SSC 983 <https://github.com/NatLabRockies/ssc/pull/983>`__)

- Detailed PV

  - Fix issue with reporting of rear-side soiling losses for bifacial modules when rack shading loss set to zero. (`SSC 978 <https://github.com/NatLabRockies/ssc/pull/978>`__)
  - Fix issue with reporting of bifacial gain output. (`SSC 951 <https://github.com/NatLabRockies/ssc/pull/951>`__)
  - Condense albedo out-of-range simulation warnings. (`SSC 996 <https://github.com/NatLabRockies/ssc/pull/996>`__)
  - Make inverter voltage ratings editable and improve system sizing messages. (`SAM 1352 <https://github.com/NatLabRockies/SAM/pull/1352>`__)
  - Fix ground clearance height issue for Sandia module model that cause simulation to stop without finishing. (`SSC 960 <https://github.com/NatLabRockies/ssc/pull/960>`__)
  - Update default module parameters for Sandia, CEC with User Specifications, Simple Efficiency, and IEC 61853 module models to be consistent with CEC Database model. (`SAM 1335 <https://github.com/NatLabRockies/SAM/pull/1335>`__

- PVWatts

  - Fix REopt API call for PVWatts-Battery configurations. (`SSC 952 <https://github.com/NatLabRockies/ssc/pull/952>`__, `SAM 1301 <https://github.com/NatLabRockies/SAM/pull/1301>`__)
  - New PVWatts warning when bifacial module enabled with fixed roof mount. (`SSC 953 <https://github.com/NatLabRockies/ssc/pull/953>`__)
  - Change default O&M cost for PVWatts-Battery configurations to better account for battery cost. (`SAM 1326 <https://github.com/NatLabRockies/SAM/pull/1326>`__)>
  - Fix callback error with PVWatts Advanced Inputs for PVWatts with no financial model configurations. (`SAM 1309 <https://github.com/NatLabRockies/SAM/pull/1309>`__)

- Marine Energy

  - Fix Marine Energy callback error when choosing resource options. (`SAM 1304 <https://github.com/NatLabRockies/SAM/pull/1304>`__)
  - Fix Marine Energy issue copying and pasting tidal resource data on Mac. (`WEX 148 <https://github.com/NatLabRockies/wex/pull/148>`__)

.. _2022-11-21:

SAM 2022.11.21, SSC 278: November 21, 2022
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This new version introduces a new Pumped Thermal Energy Storage (PTES) model and consists of several improvements and updates. For a complete list of code revisions associated with this version, see the `SAM <https://github.com/NatLabRockies/SAM/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2022+Release%22+is%3Aclosed>`__ and `SSC <https://github.com/NatLabRockies/ssc/pulls?q=is%3Apr+milestone%3A%22SAM+Fall+2022+Release%22+is%3Aclosed>`__ GitHub repositories. Highlights include:

- New Pumped Thermal Energy Storage (PTES) model with dispatch optimization.
- CSP Power Tower option to charge TES (via dispatch optimization) from grid via electric heater.
- PV uncertainty model improves on P50/P90 simulations for photovoltaic systems.
- Add battery storage and PPA Single Owner financial model option to Marine Energy Wave model.
- Updates to Geothermal Power model.
- Spatial albedo and ground irradiance calculations for Detailed Photovoltaic model.
- Investment tax credit (ITC) as either Year 1 credit or spread over multiple years.

The following is a list of outstanding issues we plan to address for an update to this version in early 2023. For a complete list see `SAM <https://github.com/NatLabRockies/SAM/issues?page=1&q=is%3Aopen+is%3Aissue+milestone%3A%22SAM+Fall+2022+Release+Patch+1%22>`__ and `SSC <https://github.com/NatLabRockies/ssc/issues?page=1&q=is%3Aopen+is%3Aissue+milestone%3A%22SAM+Fall+2022+Release+Patch+1%22>`__ repositories:

- Add support for ITC as array to cash flow send-to-Excel with equations.
- Generic System model Generate profile from open cases causes SAM to crash.
- Behind-the-meter (BTM) battery does not dispatch for small battery.
- Update Cambium market price downloads to 2021 data.
- Grid interconnection limit does not affect monthly outputs.
- Update WIND Toolkit downloads.
- Fuel cell with battery simulations progress bar to 133%.

General (2022.11.21)
--------------------

- Update defaults for current version of `NREL Annual Technology Baseline (ATB) <https://atb.nrel.gov/>`__
- Fix user interface layout issues for Linux and Mac versions. (This is an ongoing effort.)
- Fix sample script for downloading weather files from NSRDB to include time zone. (`SAM 1213 <https://github.com/NatLabRockies/SAM/pull/1213>`__)
- Fix issue with solar resource library folder with Advanced Download option. (`SAM 1182 <https://github.com/NatLabRockies/SAM/pull/1182>`__)
- Improve Edit Losses window to support specifying time periods with minutes. (`SAM 1164 <https://github.com/NatLabRockies/SAM/pull/1164>`__, `SSC 878 <https://github.com/NatLabRockies/ssc/pull/878>`__)
- For array inputs, automatically convert inputs provided as number to array. This impacts LK scripts, Software Development Kit, and PySAM users, and is intended to provide backward compatibility for code written for ITC as number. (`SSC 907 <https://github.com/NatLabRockies/ssc/pull/907>`__)

Financial Models (2022.11.21)
-----------------------------

- Fix bug in Community Solar subscriber NPV calculation. (`SAM 1242 <https://github.com/NatLabRockies/SAM/pull/1242>`__, `SSC 929 <https://github.com/NatLabRockies/ssc/pull/929>`__)
- Change investment tax credit (ITC) inputs from number to array to allow it to be specified for one year or spread out over multiple years. (`SSC 913 <https://github.com/NatLabRockies/ssc/pull/913>`__)

Electricity Rates (2022.11.21)
------------------------------

- For Utility Rate Database (URDB) downloads, apply look back months to billing demand inputs instead of listing as unused. (`SAM 1237 <https://github.com/NatLabRockies/SAM/pull/1237>`__)
- Fix rollover calculation for bill without system. (`SSC 941 <https://github.com/NatLabRockies/ssc/pull/941>`__)

Detailed Photovoltaic (2022.11.21)
----------------------------------

- Update module and inverter libraries. (`SAM 1269 <https://github.com/NatLabRockies/SAM/pull/1269>`__)
- Enhance bifacial model with electrical mismatch loss estimate and spatial albedo, and new Spatial tab on Results page. (`SAM 1152 <https://github.com/NatLabRockies/SAM/pull/1152>`__, `SSC 871 <https://github.com/NatLabRockies/ssc/pull/871>`__, `SAM 1199 <https://github.com/NatLabRockies/SAM/pull/1199>`__)
- Update P50/P90 simulations to PV Uncertainty with more sophisticated modeling of uncertainty factors. (`SAM 1076 <https://github.com/NatLabRockies/SAM/pull/1076>`__)
- Add option to input land area in acres/MWac and fix land area calculatoins. (`SAM 1195 <https://github.com/NatLabRockies/SAM/pull/1195>`__, `SAM 1166 <https://github.com/NatLabRockies/SAM/pull/1166>`__)
- Fix problem with terrain slope input enabling. (`SAM 1191 <https://github.com/NatLabRockies/SAM/pull/1191>`__)
- Improve error handling for different albedo input options. (`SAM 1180 <https://github.com/NatLabRockies/SAM/pull/1180>`__)
- Add warning in user interface for negative tilt angles. (`SAM 1171 <https://github.com/NatLabRockies/SAM/pull/1171>`__)
- Report latitude, longitude, elevation, and time zone from weather file header in simulation results. (`SSC 908 <https://github.com/NatLabRockies/ssc/pull/908>`__)
- New time series module temperature input to bypass internal calculations (SSC and LK script only, not available in SAM input pages). (`SSC 897 <https://github.com/NatLabRockies/ssc/pull/897>`__)
- Improve input checks in variable table for Software Development Kit and PySAM users. (`SSC 885 <https://github.com/NatLabRockies/ssc/pull/885>`__)
- Fix issue with AC wiring loss for subhourly simulations. (`SSC 879 <https://github.com/NatLabRockies/ssc/pull/879>`__)

PVWatts (2022.11.21)
--------------------

- Improve handling and error reporting for albedo. (`SAM 1136 <https://github.com/NatLabRockies/SAM/pull/1136>`__, `SSC 864 <https://github.com/NatLabRockies/ssc/pull/864>`__)
- Add option to input land area in acres/MWac and fix land area calculatoins. (`SAM 1195 <https://github.com/NatLabRockies/SAM/pull/1195>`__, `SAM 1166 <https://github.com/NatLabRockies/SAM/pull/1166>`__)
- Report latitude, longitude, elevation, and time zone from weather file header in simulation results. (`SSC 908 <https://github.com/NatLabRockies/ssc/pull/908>`__)
- Fix issue with snow loss model that resulted in DC snow loss of zero for time steps with snow. (`SSC 874 <https://github.com/NatLabRockies/ssc/pull/874>`__)
- Fix issue with inverter efficiency at lower power. (`SSC 872 <https://github.com/NatLabRockies/ssc/pull/872>`__)

Battery Storage (2022.11.21)
----------------------------

- Improve reporting of AC and DC in kW and kWh inputs and outputs. (`SAM 1183 <https://github.com/NatLabRockies/SAM/pull/1183>`__, `SSC 899 <https://github.com/NatLabRockies/ssc/pull/899>`__, `SSC 889 <https://github.com/NatLabRockies/ssc/pull/889>`__)
- Improve passing of SAM inputs to REopt. (`SAM 1165 <https://github.com/NatLabRockies/SAM/pull/1165>`__)
- Improve transformer loss calculations so losses are better accounted for in battery dispatch. (` <https://github.com/NatLabRockies/ssc/pull/936>`__)
- For PV smoothing, change battery state of charge (SOC) to end of time step to be consistent with other dispatch options. (`SSC 904 <https://github.com/NatLabRockies/ssc/pull/904>`__)
- Restore PV priority option for manual dispatch option. (`SSC 900 <https://github.com/NatLabRockies/ssc/pull/900>`__)
- Various Levelized Cost of Storage (LCOS) fixes. (`SSC 883 <https://github.com/NatLabRockies/ssc/pull/883>`__, `SSC 853 <https://github.com/NatLabRockies/ssc/issues/853>`__)
- Fix price signals dispatch option for rates with different periods in a month. (`SSC 882 <https://github.com/NatLabRockies/ssc/pull/882>`__)
- Make front-of-meter (FOM) automated dispatch aware of retail electricity prices. (`SSC 824 <https://github.com/NatLabRockies/ssc/issues/824>`__)
- Improvements to front-of-meter (FOM) automated dispatch. (`SSC 877 <https://github.com/NatLabRockies/ssc/pull/877>`__)
- New 'utilityrateforecast' compute module for Software Development Kit (SDK) and PySAM coders. (`SSC 868 <https://github.com/NatLabRockies/ssc/pull/868>`__)

Concentrating Solar Power (2022.11.21)
--------------------------------------

- Remove boiler pressure input from Rankine cycle model. (`SAM 1192 <https://github.com/NatLabRockies/SAM/pull/1192>`__)
- Bug fixes for Rankine power cycle model. These changes result in up to 5% increase in total annual energy production for the default molten salt power tower model, and smaller increases for other CSP models using this power cycle model. (`SSC 861 <https://github.com/NatLabRockies/ssc/pull/861>`__)

Marine Energy
-------------

- Add battery storage option to Marine Wave model. (`SSC 890 <https://github.com/NatLabRockies/ssc/pull/890>`__)

Geothermal Power (2022.11.21)
-----------------------------

- update to latest version of GETEM. (`SAM 1174 <https://github.com/NatLabRockies/SAM/pull/1174>`__, `SSC 886 <https://github.com/NatLabRockies/ssc/pull/886>`__)
- Fix number of wells calculation. (`SSC 875 <https://github.com/NatLabRockies/ssc/pull/875>`__)

Wind Power (2022.11.21)
-----------------------

- Remove BOS model options from Residential and Commercial configurations. (`SAM 1169 <https://github.com/NatLabRockies/SAM/pull/1169>`__)

Fuel Cell
---------

- Fix issue with fuel cell capex table interpolation. (`SAM 1272 <https://github.com/NatLabRockies/SAM/pull/1272>`__)
- Allow AC-connected battery to charge from fuel cell during grid outage. (`SSC 880 <https://github.com/NatLabRockies/ssc/pull/880>`__)

.. _2021-12-02-r2:

SAM 2021.12.02 r2, SSC 274: June 6 , 2022
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to :ref:`SAM 2021.12.02 Revision 1 <2021-12-02>`.

- Improve feedback for parametric simulation graphs when they cannot be displayed. (`1051 <https://github.com/NatLabRockies/SAM/pull/1051>`__)
- Change labels on Results page Metrics table and graphs to be more consistent with output variable labels. (`1045 <https://github.com/NatLabRockies/SAM/pull/1045>`__)
- Fix PVWatts land area value shown on cost input pages for PVWatts Battery configurations. (`1037 <https://github.com/NatLabRockies/SAM/pull/1037>`__)
- Fix issue with Data Tables tab on Results page to correctly group variables by number of rows. (`1001 <https://github.com/NatLabRockies/SAM/pull/1001>`__)
- For Wind resource data download from WIND Toolkit, add 2014 to list of available years. (`963 <https://github.com/NatLabRockies/SAM/pull/963>`__)
- Revise Help system for latest updates. (`1062 <https://github.com/NatLabRockies/SAM/pull/1062>`__)
- Add PVWatts output for AC power before AC adjustment factors are applied. (`SSC 830 <https://github.com/NatLabRockies/ssc/pull/830>`__)
- Fix utilityrate module descriptions to clarify dependence on different OpenEI URDB versions. (`SSC 829 <https://github.com/NatLabRockies/ssc/pull/829>`__)
- Detailed PV

  - Update CEC module and inverter libraries to May 2022 CEC database versions. (`1057 <https://github.com/NatLabRockies/SAM/pull/1057>`__, `1067 <https://github.com/NatLabRockies/SAM/pull/1067>`__)
  - Fix problem with DC degradation inputs that caused a simulation error "Degradation calculated to be greater than 100%, capped at 100%". `958 <https://github.com/NatLabRockies/SAM/pull/958>`__, `SSC 772 <https://github.com/NatLabRockies/ssc/pull/772>`__)
  - Add button to Inverter page Inverter Datasheet option to import library parameters to datasheet inputs. (`1053 <https://github.com/NatLabRockies/SAM/pull/1053>`__)
  - Fix inverter temperature derate table issue to provide feedback instead of generating an error when table has wrong number of columns. (`1027 <https://github.com/NatLabRockies/SAM/pull/1027>`__)
  - Fix PVsyst shade data import for Version 7. (`1054 <https://github.com/NatLabRockies/SAM/pull/1054>`__)
  - Fix PVsyst PAN to SAM macro to bettery handle module technology parameter. (`1047 <https://github.com/NatLabRockies/SAM/pull/1047>`__)
  - Fix Solmetric Suneye shading import. (`1052 <https://github.com/NatLabRockies/SAM/pull/1052>`__)
  - Add transmission losses to loss diagram. (`1000 <https://github.com/NatLabRockies/SAM/pull/1000>`__, `SSC 790 <https://github.com/NatLabRockies/ssc/pull/790>`__)
  - Make 6parsolve module coefficient calculator error messages more descriptive. (`SSC 827 <https://github.com/NatLabRockies/ssc/pull/827>`__, `802 <https://github.com/NatLabRockies/ssc/pull/802>`__)
  - Improve handling of albedo data errors. (`SSC 799 <https://github.com/NatLabRockies/ssc/pull/799>`__)
  - Report time series outputs over lifetime to facilitate comparison with other outputs in results. (`SSC 791 <https://github.com/NatLabRockies/ssc/pull/791>`__)

- Battery Storage

  - Fix REopt battery sizing and dispatch API calls. (`999 <https://github.com/NatLabRockies/SAM/pull/999>`__)
  - Add new battery dispatch input for Generic Battery configurations to allow battery to dispatch to a different generation profile than the power system generation profile. (`992 <https://github.com/NatLabRockies/SAM/pull/992>`__)
  - Fix problem with inputs for battery dispatch to grid power targets option. (`988 <https://github.com/NatLabRockies/SAM/pull/988>`__)
  - Fix issue with voltage table outside of battery state-of-charge limits. (`SSC 883 <https://github.com/NatLabRockies/ssc/pull/833>`__)
  - Add metric for capacity factor based on AC power to Standalone Battery configurations. (`1050 <https://github.com/NatLabRockies/SAM/pull/1050>`__, `SSC 820 <https://github.com/NatLabRockies/ssc/pull/820>`__)
  - Fix Standalone Battery nameplate capacity value used for capacity-based incentive and capacity revenue calculations. (`1040 <https://github.com/NatLabRockies/SAM/pull/1040>`__)
  - Add a new LK sample script to replicate battery computed properties calculations from SAM's user interface.(`1002 <https://github.com/NatLabRockies/SAM/pull/1002>`__)
  - Fix PV Battery / Single Owner default value of batt_Vcut to match other battery configurations. (`1032 <https://github.com/NatLabRockies/SAM/pull/1032>`__)
  - Make Detailed PV macros available for PV Battery configurations, including for PV system sizing, PVsyst file import, subarray layout, etc. (`967 <https://github.com/NatLabRockies/SAM/pull/967>`__)
  - Apply battery state-of-charge limits to PV smoothing battery dispatch option. (`SSC 816 <https://github.com/NatLabRockies/ssc/pull/816>`__)
  - Improve behind-the-meter price signal dispatch option to respond to demand ratchets in electricty rate structure. (`SSC 800 <https://github.com/NatLabRockies/ssc/pull/800>`__)
  - Improve front-of-meter battery dispatch to price forecast to consider battery capacity in addition to price forecast. (`SSC 792 <https://github.com/NatLabRockies/ssc/pull/792>`__)
  - Improve front-of-meter battery dispatch to price forecast to use lifetime price data. (`SSC 787 <https://github.com/NatLabRockies/ssc/pull/787>`__)
  - Improve the way daily losses are applied to battery power. (`SSC 780 <https://github.com/NatLabRockies/ssc/pull/780>`__)
  - Account for battery operating costs in behind-the-meter (BTM) and front-of-meter (FOM) dispatch. (`SSC 856 <https://github.com/NatLabRockies/ssc/pull/856>`__)

- Concentrating Solar Power (CSP)

  - Fix physical trough bug when number of SCAs in loop is changed. (`1014 <https://github.com/NatLabRockies/SAM/pull/1014>`__)
  - Fix issues with custom HTF. (`1007 <https://github.com/NatLabRockies/SAM/pull/1007>`__, `1006 <https://github.com/NatLabRockies/SAM/pull/1006>`__)
  - Add sub-cooled outlet option to Industrial Process Heat (IPH) Direct Steam Linear Fresnel model. (`973 <https://github.com/NatLabRockies/SAM/pull/973>`__, `SSC 776 <https://github.com/NatLabRockies/ssc/pull/776>`__)
  - Fix issue with Molten Salt Linear Fresnel (MSLF) partial sequential defocus. (`SSC 759 <https://github.com/NatLabRockies/ssc/pull/759>`__)

- Marine Energy

  - Add new feature to convert a set of one-year time series wave resource data files into a single joint probability distribution (JPD) file. (`993 <https://github.com/NatLabRockies/SAM/pull/993>`__)
  - For Wave Energy model, improve layout and text on Wave Resource input page. (`1025 <https://github.com/NatLabRockies/SAM/pull/1025>`__)

- Financial Models

  - Fix Send to Excel with Equations for all financial models so that Excel results more closely match SAM results. (`1044 <https://github.com/NatLabRockies/SAM/pull/1044>`__, `SSC 818 <https://github.com/NatLabRockies/ssc/pull/818>`__)
  - Fix issue with Value of RE macro tax rate calculation that prevented macro from running. (`982 <https://github.com/NatLabRockies/SAM/pull/982>`__)
  - Edit variable labels to include abbreviations LCOE, NPV, and IRR to facilitate searching for variable names. (`954 <https://github.com/NatLabRockies/SAM/pull/954>`__, `SSC 770 <https://github.com/NatLabRockies/ssc/pull/770>`__)
  - Fix merchant plant price data download from Cambium API. (`1063 <https://github.com/NatLabRockies/SAM/pull/1063>`__)
  - Fix merchant plant cleared capacity calculations when capacity is negative for battery charging or for system self-consumption. (`1059 <https://github.com/NatLabRockies/SAM/pull/1059>`__, `SSC 826 <https://github.com/NatLabRockies/ssc/pull/826>`__)
  - Fix interaction between analysis period and merchant plant price data input window. (`1041 <https://github.com/NatLabRockies/SAM/pull/1041>`__)
  - Fix LCOE Calculator issue with undefined analysis period and no financial model. (`SSC 797 <https://github.com/NatLabRockies/ssc/pull/797>`__)

- Solar Resource Data

  - Update Solar Resource File Converter macro for SolarAnywhere data to better handle subhourly data and missing data at end of row. (`1019 <https://github.com/NatLabRockies/SAM/pull/1019>`__, `951 <https://github.com/NatLabRockies/SAM/pull/951>`__)
  - Fix NSRDB weather data queries from Advanced NSRDB window to return correct time zone. (`1023 <https://github.com/NatLabRockies/SAM/pull/1023>`__, `950 <https://github.com/NatLabRockies/SAM/pull/950>`__)
  - Improve messages when NSRDB API returns an error message instead of weather data. (`1018 <https://github.com/NatLabRockies/SAM/pull/1018>`__)
  - Improve wfcheck error time stamp reporting. (`SSC 773 <https://github.com/NatLabRockies/ssc/pull/773>`__)

.. _2021-12-02-r1:

SAM 2021.12.02 r1, SSC 274: February 21, 2022
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to :ref:`SAM 2021.12.02 <2021-12-02>`.

- Update CEC module and inverter libraries to December 2021 and February 2022 CEC database versions, respectively. (`946 <https://github.com/NatLabRockies/SAM/pull/946>`__)
- Change default sales tax value to same value for all configurations. (`844 <https://github.com/NatLabRockies/SAM/pull/844>`__)
- Fix issues with Solar Resource location name parsing of latitude/longitude pairs, addresses, and location names. (`929 <https://github.com/NatLabRockies/SAM/pull/929>`__)
- Allow PVLIB labels as solar resource data columns in SAM CSV weather file format. (`SSC 754 <https://github.com/NatLabRockies/ssc/pull/754>`__)
- Fix land area calculations for PVWatts and Detailed Photovoltaic models. (`928 <https://github.com/NatLabRockies/SAM/pull/928>`__, `SSC 761 <https://github.com/NatLabRockies/ssc/pull/761>`__)
- Revise Help topics and fix issues with context-sensitive ETES Help topics not displaying properly. (`938 <https://github.com/NatLabRockies/SAM/pull/938>`__)
- Fix marine energy labels in user interface. (`916 <https://github.com/NatLabRockies/SAM/pull/916>`__)
- New marine wave energy option to convert time series wave resource data file to joint probability distribution data file. (`892 <https://github.com/NatLabRockies/SAM/pull/892>`__)
- Update geothermal drilling cost curves. (`910 <https://github.com/NatLabRockies/SAM/pull/910>`__)
- Fix geothermal issue with time stamps for hourly simulation option. (`SSC 690 <https://github.com/NatLabRockies/ssc/pull/690>`__
- Update electric thermal energy storage (ETES) default tax incentives and debt mode. (`900 <https://github.com/NatLabRockies/SAM/pull/900>`__)
- Fix issue with wind turbine characteristics equations. (`894 <https://github.com/NatLabRockies/SAM/pull/897>`__, `SSC 730 <https://github.com/NatLabRockies/ssc/pull/730>`__)
- Allow wind resource files with "srw" and "csv" extension. (`929 <https://github.com/NatLabRockies/SAM/pull/929>`__)
- Update SAM sample files to work with SAM 2021.12.01. (`895 <https://github.com/NatLabRockies/SAM/pull/895>`__)
- Cash Flow

  - Fix Cash Flow Send to Excel with Equations for Single Owner financial model. This feature will be fixed for other financial models for SAM 2021.12.02 Revision 2. (`936 <https://github.com/NatLabRockies/SAM/pull/936>`__)
  - Add operation and maintenance costs for battery, fuel cell, and fuel as appropriate to cash flow operating costs line itames. (`927 <https://github.com/NatLabRockies/SAM/pull/927>`__, `909 <https://github.com/NatLabRockies/SAM/pull/909>`__, `SSC 762 <https://github.com/NatLabRockies/ssc/pull/762>`__, `SSC 753 <https://github.com/NatLabRockies/ssc/pull/753>`__)
  - Fix annual electricity values for cash flow output. (`919 <https://github.com/NatLabRockies/SAM/pull/919>`__, `SSC 756 <https://github.com/NatLabRockies/ssc/pull/756>`__, `SSC 750 <https://github.com/NatLabRockies/ssc/pull/750>`__)

- Battery

  - Fix issue with battery environment temperature input. (`920 <https://github.com/NatLabRockies/SAM/pull/920>`__, `SSC 751 <https://github.com/NatLabRockies/ssc/pull/751>`__)
  - Fix issues with battery dispatch inputs including enabling/disabling options, and grid power target array length. (`860 <https://github.com/NatLabRockies/SAM/pull/860>`__, `855 <https://github.com/NatLabRockies/SAM/pull/855>`__, `858 <https://github.com/NatLabRockies/SAM/pull/858>`__)
  - Fix battery life model for Li-ion NMC/Graphite battery type. (`SSC 763 <https://github.com/NatLabRockies/ssc/pull/763>`__, `748 <https://github.com/NatLabRockies/ssc/pull/748>`__)
  - Fix battery dispatch PV smoothing algorithm to make battery state of charge value for dispatch consistent with value for simulation. (`SSC 758 <https://github.com/NatLabRockies/ssc/pull/758>`__)
  - Fix issue with battery merchant plant dispatch to power price and check that generation exceeds cleared capacity. (`SSC 729 <https://github.com/NatLabRockies/ssc/pull/729>`__)
  - Change "Market sell rate" label to "Power price for battery dispatch" to make it clear variable is for battery dispatch calculations. (`SSC 706 <https://github.com/NatLabRockies/ssc/pull/706>`__)
  - Fix battery degradation issue that caused SAM to crash. (`SSC 796 <https://github.com/NatLabRockies/ssc/pull/796>`__)

- Grid Outage and Critical Load

  - Consolidate inputs for behind-the-meter grid outage inputs on new Grid Outage page. (`902 <https://github.com/NatLabRockies/SAM/pull/902>`__, `842 <https://github.com/NatLabRockies/SAM/pull/842>`__)
  - Remove grid outage feature from PVWatts - battery configuration. (`884 <https://github.com/NatLabRockies/SAM/pull/884>`__, `945 <https://github.com/NatLabRockies/SAM/pull/945>`__, `SSC 767 <https://github.com/NatLabRockies/ssc/pull/767>`__)
  - Add grid outage inputs to standalone battery behind-the-meter configurations. (`873 <https://github.com/NatLabRockies/SAM/pull/873>`__)
  - Fix grid outage issue to prevent load during outage being used for electricity bill calculation. (`SSC 741 <https://github.com/NatLabRockies/ssc/pull/741>`__)
  - Improve behavior of DC-connected battery during outage to allow charging during outage. (`SSC 738 <https://github.com/NatLabRockies/ssc/pull/738>`__, `SSC 728 <https://github.com/NatLabRockies/ssc/pull/728>`__, `SSC 725 <https://github.com/NatLabRockies/ssc/pull/725>`__)
  - Fix problem with scaling of critical load. (`842 <https://github.com/NatLabRockies/SAM/pull/842>`__)

- Concentraing Solar Power (CSP)

  - Fix CSP molten salt linear Fresnel thermal efficiency calculation in user interface. (`939 <https://github.com/NatLabRockies/SAM/pull/939>`__)
  - Fix CSP molten salt linear Fresnel sequential defocus bug. (`SSC 760 <https://github.com/NatLabRockies/ssc/pull/760>`__)
  - Fix CSP user-defined power cycle equation for HTF mass flow rate. When applied to the default molten salt power tower case where the cycle model is changed to the default UDPC inputs, the annual energy increase 0.1%. The effect may be more significant if 1) your UDPC data includes more variation relative to the default data at m_dot_HTF > 1 or 2) your case increases the maximum HTF over-design above the default 1.05 value. (`SSC 695 <https://github.com/NatLabRockies/ssc/pull/695>`__)
  - Update default financial assumptions for concentrating solar power (CSP) configurations. (`904 <https://github.com/NatLabRockies/SAM/pull/904>`__)

- Electricity Bill and Purchases

  - Fix issue with front-of-meter electricity purchases when non-zero sell rate input value was hidden. (`908 <https://github.com/NatLabRockies/SAM/pull/908>`__)
  - Fix issues with billing demand periods table. (`889 <https://github.com/NatLabRockies/SAM/pull/889>`__)
  - Fix issues with billing demand calculations with only kWh/kW energy charge units. (`SSC 723 <https://github.com/NatLabRockies/ssc/pull/723>`__)
  - Fix issue with reporting of time series demand charge and electricity peak values. (`SSC 733 <https://github.com/NatLabRockies/ssc/pull/733>`__)

.. _2021-12-02:

SAM 2021.12.02, SSC 267: December 2, 2021
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This new version consists of several improvements and updates. Highlights include:

- Community Solar financial model
- PVWatts update from V7 to V8
- Standalone Battery and Standalone Electric Thermal Energy Storage models
- Cavity receiver model for power towers
- PV smoothing battery dispatch option and multiple improvements to battery dispatch and degradation models
- Levelized cost of storage (LCOS) metric for battery storage configurations
- Grid outage for systems with battery storage
- Billing demand with lookback period for kWh/kW energy rates and ratcheting demand rates

General (2021.12.02)
--------------------

- Fix issues with PDF report templates for battery storage configurations, PPA financial models, and marine energy configurations. (`822 <https://github.com/NatLabRockies/SAM/pull/822>`__)
- Update default assumptions to match `NREL Annual Technology Baseline <https://atb.nrel.gov/>`__. (`833 <https://github.com/NatLabRockies/SAM/pull/833>`__, `827 <https://github.com/NatLabRockies/SAM/issues/827>`__, `823 <https://github.com/NatLabRockies/SAM/pull/823>`__, `812 <https://github.com/NatLabRockies/SAM/pull/812>`__, `793 <https://github.com/NatLabRockies/SAM/pull/793>`__, `786 <https://github.com/NatLabRockies/SAM/pull/786>`__, `783 <https://github.com/NatLabRockies/SAM/pull/783>`__)
- Fix issue with yellow page notes disappearing when opening files created with older versions of SAM. (`809 <https://github.com/NatLabRockies/SAM/pull/809>`__)
- Replace Combine Cases macro with option on Generic System model Power Plant input age. (`706 <https://github.com/NatLabRockies/SAM/pull/706>`__, `765 <https://github.com/NatLabRockies/SAM/pull/765>`__)

PVWatts (2021.12.02)
--------------------

With this release, we have updated SAM's implementation to PVWatts V8 (pvwattsv8 in SSC) with new features listed below. The online PVWatts® Calculator continues to use pvwattsv5 and will be updated later. All versions of PVWatts are available via the SAM Software Development Kit and PySAM. (`729 <https://github.com/NatLabRockies/SAM/pull/729>`__)

- Reorganize user interface to group advanced inputs into a collapsible panel.
- Add snow loss model when weather file includes snow depth data in cm.
- Support for bifacial modules.
- Internal module and module thermal models based on same model as Detailed PV model.
- Internal inverter model based on same model as Detailed PV model.
- Inputs for monthly soiling as DC loss.

Detailed Photovoltaic (2021.12.02)
----------------------------------

- Modify default inverter temperature derate curve to work over the full range of inverter sizes in the CEC inverter library. (`804 <https://github.com/NatLabRockies/SAM/pull/804>`__)
- Fix Shade Calculator crashing on MacOS. (`775 <https://github.com/NatLabRockies/SAM/pull/775>`__)
- Add terrain slope inputs for one-axis trackers to improve backtracking and linear self-shading calculations for large arrays on sloped land. (`742 <https://github.com/NatLabRockies/SAM/pull/742>`__)
- Model PV degradation as linear instead of compounded annually to better reflect recent research on module degradation. (`756 <https://github.com/NatLabRockies/SAM/pull/756>`__)
- For weather files that generate simulation messages about abnormal irradiance values, change messages from warnings to notices to avoid long list of messages after simulation. (`658 <https://github.com/NatLabRockies/ssc/pull/658>`__)
- Update the CEC module model to assume anti-reflective coatings for all PV modules (`662 <https://github.com/NatLabRockies/ssc/pull/662>`__)

Battery Storage (2021.12.02)
----------------------------

- Redesigned user interface for battery dispatch choices for PV Battery, Generic Battery, Fuel Cell, and new Standalone Battery configurations. (`819 <https://github.com/NatLabRockies/SAM/pull/819>`__, `834 <https://github.com/NatLabRockies/SAM/pull/834>`__, `830 <https://github.com/NatLabRockies/SAM/pull/830>`__, `821 <https://github.com/NatLabRockies/SAM/pull/821>`__, `819 <https://github.com/NatLabRockies/SAM/pull/819>`__, `806 <https://github.com/NatLabRockies/SAM/pull/806>`__, `805 <https://github.com/NatLabRockies/SAM/pull/805>`__, `712 <https://github.com/NatLabRockies/SAM/pull/712>`__, `690 <https://github.com/NatLabRockies/SAM/pull/690>`__, `656 <https://github.com/NatLabRockies/SAM/pull/656>`__, `644 <https://github.com/NatLabRockies/SAM/pull/644>`__)
- Add option to specify surface area of single battery to improve battery temperature calculations for large batteries.(`789 <https://github.com/NatLabRockies/SAM/pull/789>`__)
- New levelized cost of storage (LCOS) metric. (`679 <https://github.com/NatLabRockies/ssc/pull/679>`__, `529 <https://github.com/NatLabRockies/SAM/pull/529>`__)
- Better battery degradation modeling for Li-ion NMC/Graphite and Li-ion LMO/LTO battery types, and improve cycle counting algorithm for battery degradation model to better handle situations with high-frequency low-DOD discharges. (`677 <https://github.com/NatLabRockies/ssc/pull/677>`__, `707 <https://github.com/NatLabRockies/SAM/pull/707>`__)
- New Standalone Battery configuration based on Generic Battery configuration. (`766 <https://github.com/NatLabRockies/SAM/pull/766>`__, `659 <https://github.com/NatLabRockies/ssc/pull/659>`__)
- Add options for AC-connected behind-the-meter batteries to charge when system output is less than load, and discharge when load is less than system output to be consistent with REopt dispatch. (`692 <https://github.com/NatLabRockies/SAM/pull/692>`__)
- Add dispatch options for behind-the-meter batteries to forecast to custom time series data for weather and load. (`688 <https://github.com/NatLabRockies/SAM/pull/688>`__, `681 <https://github.com/NatLabRockies/SAM/pull/681>`__)
- Allow behind-the-meter batteries to be charged from clipped photovoltaic power, and to discharge to the grid. (`754 <https://github.com/NatLabRockies/SAM/pull/754>`__, `653 <https://github.com/NatLabRockies/ssc/pull/653>`__)
- Remove option to use ambient temperature data from weather file as battery environment temperature for battery temperature calculations because Generic Battery and Standalone Battery configurations do not have a weather file. SAM automatically applies weather file ambient temperature to battery environment time series input when opening older files as appropriate. (`679 <https://github.com/NatLabRockies/SAM/pull/679>`__)
- Add grid outage dispatch capability for behind-the-meter batteries. (`722 <https://github.com/NatLabRockies/SAM/pull/722>`__, `721 <https://github.com/NatLabRockies/SAM/pull/721>`__, `707 <https://github.com/NatLabRockies/SAM/pull/707>`__)
- Ensure correct battery capacity values are read into new variables when opening a file or case created by an older version of SAM. (`781 <https://github.com/NatLabRockies/SAM/pull/781>`__)
- Ensure correct values passed to model when switching between kWh and h option for specifying battery capacity. (`720 <https://github.com/NatLabRockies/SAM/pull/720>`__)
- Disable option for charging from photovoltaic clipped power for Generic Battery configurations. (`677 <https://github.com/NatLabRockies/SAM/pull/677>`__)
- New cutoff voltage input for battery voltage model. (`502 <https://github.com/NatLabRockies/SAM/pull/502>`__)
- Fix issue with cycle degradation penalty value of zero or NaN ("not a number") calculation for price signal and automated economic dispatch options. (`627 <https://github.com/NatLabRockies/ssc/pull/627>`__)

Financial Models (2021.12.02)
-----------------------------

- Separate System Costs page into Installation Costs and Operating Costs pages to improve visbility of operating cost inputs and allow for more flexibilty in operating cost inputs for different technology types. (`653 <https://github.com/NatLabRockies/SAM/pull/653>`__)
- Treat electricity purchases to charge batteries or meet night-time load as an operating cost in project cash flow regardless of whether power price for purchases is retail rate or PPA price. (`785 <https://github.com/NatLabRockies/SAM/pull/785>`__)
- Add option to front-of-meter financial models to model land lease payments as an operating cost in project cash flow. (`779 <https://github.com/NatLabRockies/SAM/pull/779>`__)
- Update electricity rate data download from NREL Utility Rate Database (URDB) to URDB Version 8 and correctly handle unsupported inputs and units. (`758 <https://github.com/NatLabRockies/SAM/pull/758>`__)
- Calculate billing demand in kW for electricity bill energy charges with kWh/kW units. (`627 <https://github.com/NatLabRockies/SAM/pull/627>`__)
- Ratcheting demand charges for electricity bill calculations. (`637 <https://github.com/NatLabRockies/ssc/pull/637>`__, `728 <https://github.com/NatLabRockies/SAM/pull/728>`__)
- Modify levelized cost of energy (LCOE) to use energy exported to grid as energy term in denominator instead of energy generated by system to better represent value of energy for systems with storage. (`749 <https://github.com/NatLabRockies/SAM/pull/749>`__)
- Fix units in variable labels for operation and maintenance (O&M) costs from $/kWh to $/MWh. (`667 <https://github.com/NatLabRockies/SAM/pull/667>`__)
- For electricity bill calculations with tiered rates and buy all/sell all compensation, ensure usage for night-time consumption (e.g., inverter night-time consumption) is assigned to the highest tier rather than the lowest tier. (`673 <https://github.com/NatLabRockies/ssc/pull/673>`__)
- Add Cambium forecast hourly price data download option to Merchant Plant model. (`0669 <https://github.com/NatLabRockies/SAM/pull/669>`__)

Concentrating Solar Power (2021.12.02)
--------------------------------------

- New cavity receiver option for the Molten Salt Power Tower model. (`625 <https://github.com/NatLabRockies/ssc/issues/625>`__).
- Fix issue with tower Heliostat Availability inputs to make clearer distinction between the design parameter and heliostat availability loss applied during simulation. (`822 <https://github.com/NatLabRockies/SAM/pull/822>`__)
- For the Molten Salt Power Tower model, change units of piping heat loss coefficient to W/m2-K from W/m. (`746 <https://github.com/NatLabRockies/SAM/issues/746>`__)
- For the Physical Trough model, improve dispatch algorithm, including modifying sizing of indirect TES (TES fluid is different from the solar field HTF): In earlier versions, we used the receiver HTF hot-to-cold temperature difference to size TES. In our current convention, we size the TES using a temperature difference = (T_rec_hot – HX_delta_T) – (T_pc_cold + HX_delta_T). This reduces the design temperature difference by 2*HX_delta_T compared to the old convention, which in turn increases the physical size of TES for a given system design. As such, the latest release effectively has more TES capacity for a given system design than older versions of SAM. ((Fix Trough Dispatch)).

Marine Energy (2021.12.02)
--------------------------

- For Wave model, add wave resource data download from NREL Wave Hindcast Data with library to store wave resource files. (`725 <https://github.com/NatLabRockies/SAM/pull/725>`__)
- New macros for Wave model to generate a report and compare cases. (`704 <https://github.com/NatLabRockies/SAM/pull/704>`__)
- For Wave model, add support for time series wave resource data. (`645 <https://github.com/NatLabRockies/SAM/pull/645>`__, `643 <https://github.com/NatLabRockies/SAM/pull/643>`__)
- Add option to import device power curve data from CSV or clipboard. (`740 <https://github.com/NatLabRockies/SAM/pull/740>`__)
- Fix issues with PDF report template, metrics table outputs, and update cost structure options. (`682 <https://github.com/NatLabRockies/SAM/pull/682>`__)

Wind (2021.12.02)
-----------------

- Fix issue with integration of Python LandBOSSE wind cost model that caused SAM to crash. (`787 <https://github.com/NatLabRockies/SAM/pull/787>`__)

Geothermal (2021.12.02)
-----------------------

- Update model to be more consistent with latest version of GETEM. (`772 <https://github.com/NatLabRockies/SAM/pull/772>`__)

.. _2020-11-29-r2:

SAM 2020.11.29 r2, SSC 256: May 12, 2021
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to :ref:`SAM 2020.11.29 Revision 1 <2020-11-29-r1>`.

Show heat map of system power generated on Summary page of Results page. (`571 <https://github.com/NatLabRockies/SAM/pull/571>`__)

Fix weather file downloads from Advanced NSRDB Download window to set "utc" parameter to false so files are returned in local time instead of UTC. (`609 <https://github.com/NatLabRockies/SAM/pull/609>`__)

Add minute value to error messages about weather files to facilitate troubleshooting problems. (`563 <https://github.com/NatLabRockies/SSC/pull/563>`__)

Change Detailed PV short circuit current output variable labels to make it clear current is string current rather than array current. (`572 <https://github.com/NatLabRockies/SSC/pull/572>`__)

Show system power generated by default on Results page time series tables and graphs instead of blank tables and graphs. (`559 <https://github.com/NatLabRockies/SAM/pull/559>`__)

Fix issue with Electricity Bill calculations that caused SAM to crash when time-of-use periods have different numbers of tiers. (`560 <https://github.com/NatLabRockies/SSC/pull/560>`__)

Improve Stochastic simulations so that input samples and results are consistent and add description of distributions to Help. (`565 <https://github.com/NatLabRockies/SAM/pull/565>`__)

For Wind power model, only display Uncertanties tab on Results page when Uncertainty inputs are enabled and improve Uncertainties input page to make it clearer that inputs are optional. (`581 <https://github.com/NatLabRockies/SAM/pull/581>`__)

Updated Solar Resource File Converter macro for SolarAnywhere files to work with latest SolarAnywhere version and fix issues with time stamp conversions. (`601 <https://github.com/NatLabRockies/SAM/pull/601>`__)

Battery model:

Reorganize battery inputs so that sizing parameters are all in one place and closer to the sizing inputs, and fix issue with sizing using DC and AC capacities. (`569 <https://github.com/NatLabRockies/SAM/pull/569>`__, `560 <https://github.com/NatLabRockies/SAM/pull/560>`__)

Add battery sizing option so battery capacity can be specified in kW and h, or kW and kWh to facilitate parametric simulations on battery capacity. (`606 <https://github.com/NatLabRockies/SAM/pull/606>`__)

Fix behind-the-meter battery dispatch issues to prevent accidental discharging of battery to grid by limiting nominal voltage to range specified in voltage table. (Not an issue for electrochemical voltage model.) (`561 <https://github.com/NatLabRockies/SAM/pull/561>`__, `568 <https://github.com/NatLabRockies/ssc/pull/568>`__)

Fix behind-the-meter battery dispatch algorithm issue that allowed discharging of battery to grid. (`513 <https://github.com/NatLabRockies/ssc/issues/513>`__)

For front-of-meter battery systems, changed the "Utility Rates" page label to "Electricity Purchases" and improved text in user interface to make it clearer that the inputs on that page are for specifying how electricity to charge the battery is purchased. (`611 <https://github.com/NatLabRockies/SAM/pull/611>`__)

Fixed issue for front-of-meter battery models that allowed retail sell rate to be non-zero so that battery discharge power was compensated at both the PPA price and sell rate instead of only at the PPA price. (`603 <https://github.com/NatLabRockies/SAM/pull/603>`__)

Remove fuel cell and battery variables from results when system does not include a fuel cell or battery. (`570 <https://github.com/NatLabRockies/SSC/pull/570>`__)

Shorten some battery output variable labels to make graphs and tables easier to work with. (`576 <https://github.com/NatLabRockies/SSC/pull/576>`__)

Fix issues with Lithium-ion NMC lifetime model's calculation of length of day for simulations with one-minute timesteps. (`549 <https://github.com/NatLabRockies/SSC/pull/549>`__, `545 <https://github.com/NatLabRockies/SSC/pull/545>`__)

Fix LK scripting issue when adding folders to the solar resource library. (`611 <https://github.com/NatLabRockies/SAM/pull/611>`__)

Revise Help to improve battery, electricity bill calculation, and other topics. (`617 <https://github.com/NatLabRockies/SAM/pull/617>`__, `602 <https://github.com/NatLabRockies/SAM/pull/602>`__)

.. _2020-11-29-r1:

SAM 2020.11.29 r1, SSC 252: February 25, 2021
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to :ref:`SAM 2020.11.29 <2020-11-29>`.

- Revert default Federal Investment Tax Credit (ITC) to 26%. (`493 <https://github.com/NatLabRockies/SAM/issues/493>`__)
- Fix power tower to correctly update tower height cost value. (`475 <https://github.com/NatLabRockies/SAM/pull/475>`__)
- Fix photovoltaic model simulation warnings for bifacial and one-axis tracking. (`364 <https://github.com/NatLabRockies/ssc/issues/364>`__)
- Battery model:

  - Fix generic battery display of power units on System Costs page. (`469 <https://github.com/NatLabRockies/SAM/issues/469>`__)
  - Fix PV-fuel-cell-battery battery O&M cost calculation. (`521 <https://github.com/NatLabRockies/SAM/pull/521>`__)
  - Make sure user interface panels on battery input pages are collapsed in a consistent way for all battery configurations. (`515 <https://github.com/NatLabRockies/SAM/pull/515>`__)
  - Fix an issue with voltage model enabling in the user interface for flow batteries. (`505 <https://github.com/NatLabRockies/SAM/issues/505>`__)
  - Fix issues with battery voltage model. (`520 <https://github.com/NatLabRockies/ssc/issues/520>`__, `517 <https://github.com/NatLabRockies/ssc/issues/517>`__)
  - Fix message about Specify IRR target for battery not being available for configurations with PPA financial models (it is). (`516 <https://github.com/NatLabRockies/SAM/issues/516>`__)
  - Fix PDF report generator for PV-Battery with PPA financial model configurations. (`528 <https://github.com/NatLabRockies/SAM/pull/528>`__)
  - Integrate new NREL NMC lifetime model (available in SSC only). (`521 <https://github.com/NatLabRockies/ssc/issues/521>`__)
  - Remove REopt optimization option from Generic Battery configurations. (`479 <https://github.com/NatLabRockies/SAM/issues/479>`__)

- Electricity bill calculation:

  - Fix electricity bill calculations to correctly apply time series buy/sell rates to bill without system. (`494 <https://github.com/NatLabRockies/SAM/issues/494>`__)
  - Fix handling of nested tiers. (`275 <https://github.com/NatLabRockies/SAM/issues/275>`__)
  - Improve reporting of monthly results. (`492 <https://github.com/NatLabRockies/SAM/issues/492>`__)

- Macros:

  - Modify Value of RE macro to support Third Party Ownership financial models. (`492 <https://github.com/NatLabRockies/SAM/issues/492>`__)
  - Fix Indifference vs PPA Price macro for Third Party Ownership finanical model. (`490 <https://github.com/NatLabRockies/SAM/issues/490>`__)
  - Fix Value of RE macro rate switching option. (`499 <https://github.com/NatLabRockies/SAM/issues/499>`__, `487 <https://github.com/NatLabRockies/SAM/issues/487>`__)
  - Update Solar Resource File converter to work with SolarAnywhere V3.4 files and older. (`491 <https://github.com/NatLabRockies/SAM/issues/491>`__)
  - Fix marine energy Compare Cases macro when comparing cases with the same LCOE. (`497 <https://github.com/NatLabRockies/SAM/issues/497>`__)
  - Remove Puerto Rico solar resource download macro now that data is available from Location and Resource page, move code to LK script samples. (`473 <https://github.com/NatLabRockies/SAM/pull/473>`__)

- Fix upgrade script issue that caused SAM to crash when opening some files created in older versions of SAM. (`518 <https://github.com/NatLabRockies/SAM/issues/518>`__)
- For Third Party Ownership model, make PPA price and lease term available for parametric simulations. (`536 <https://github.com/NatLabRockies/SAM/pull/536>`__)
- Improve handling of API errors for weather file downloads from NSRDB. (`541 <https://github.com/NatLabRockies/SAM/pull/541>`__)
- Improve Data Table categories on Results page. (`485 <https://github.com/NatLabRockies/SAM/issues/485>`__)
- Clean up Marine Energy System Costs pages. (`489 <https://github.com/NatLabRockies/SAM/pull/489>`__)
- Fix REopt inputs to remove unused value of lost load, and only set battery capacity to zero when REopt finds an optimal solution. (`482 <https://github.com/NatLabRockies/SAM/issues/482>`__, `495 <https://github.com/NatLabRockies/SAM/issues/495>`__)
- Fix P50/P90 simulations to work with weather files files in EPW format. (`480 <https://github.com/NatLabRockies/SAM/issues/480>`__)

.. _2020-11-29:

SAM 2020.11.29, SSC 250: November 29, 2020
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This new version consists of several improvements and updates. Highlights include:

- Implement NREL Solar Position Algorithm (SPA) for sun angle calculations for all solar performance models.
- Improved battery dispatch for both front-of-meter and behind-the-meter battery storage applications.
- Improve electricity bill calculations for distributed behind-the-meter financial models.
- Update utility rate data download from OpenEI Utility Rate Database (URDB) to API Version 7 and fix a bug that prevented all rates from being listed for some large utilities.
- Improved photovoltaic backtracking algorithm.
- Integrate NREL LandBOSSE wind balance-of-system (BOS) cost model into SAM.
- Update default values to better align with `NREL Annual Technology Baseline (ATB). <https://atb.nrel.gov/>`__
- Update solar resource data downloads from the NSRDB to access newest datasets for subhourly data, typical year (TMY, TGY, TDY) data, subhourly data, and data for Europe, Africa and Asia.

In the following list of changes, numbers in parentheses are links to detailed descriptions and code on the SAM or SSC GitHub repositories.

General
-------

- Fix issue with scroll bars disappearing when long pages are loaded in the user interface. (`405 <https://github.com/NatLabRockies/SAM/issues/405>`__)
- Fix problem opening files with parametric simulations that caused SAM to crash with "Could not inflate stream" error. (`393 <https://github.com/NatLabRockies/SAM/issues/393>`__)
- Fix problem with Welcome page when SAM starts without an internet connection. (`376 <https://github.com/NatLabRockies/SAM/issues/376>`__)
- Update default ITC and PTC tax credits to reflect current market conditions. (`450 <https://github.com/NatLabRockies/SAM/pull/450>`__)
- Update sun angle calculations for all solar models to use NREL Solar Position Algorithm (SPA). (`279 <https://github.com/NatLabRockies/ssc/issues/279>`__)
- For the SSC compute module pvwattsv5_1ts, add external shading. This change does not affect the PVWatts implementation in SAM, which uses the pvwattsv7 compute module. (`462 <https://github.com/NatLabRockies/ssc/pull/462>`__)

Detailed Photovoltaic
---------------------

- Improve backtracking algorithm to use method described in Anderson, K (2020), "Slope-aware Backtracking for Single Axis Trackers", NREL/TP-5K00-76626. A new slope input will be added in a future version of SAM. (`457 <https://github.com/NatLabRockies/ssc/issues/457>`__).
- Add module mass input for transient thermal model that adjusts cell temperature calculation for simulation time steps of less than 20 minutes, and improve user interface and documentation to make it clear how the model works. (`380 <https://github.com/NatLabRockies/SAM/issues/380>`__)
- Rename Array DC power variable to Inverter DC input power to avoid confusion about whether value includes DC losses. (`383 <https://github.com/NatLabRockies/SAM/issues/383>`__)
- Improve inverter temperature derate curve inputs, and normalize efficiency table to Pmax instead of efficiency. (`412 <https://github.com/NatLabRockies/SAM/pull/412>`__)
- Fix module coefficient generator for CEC Module Model with User Entered Specifications option to work with newer high-current modules. (`129 <https://github.com/NatLabRockies/ssc/issues/129>`__)
- Improve warning messages for bifacial modules with one-axis trackers and non-zero tilt. (`364 <https://github.com/NatLabRockies/ssc/issues/364>`__)
- Update default system size for PPA financial models to 50 MW and Commercial to 500 kW. (`392 <https://github.com/NatLabRockies/SAM/pull/421>`__)

Battery Storage
---------------

- Add new behind-the-meter (BTM) dispatch option that reponds to electricity rates and cycle degradation penalty.
- Add option so battery cost can be specified in $/kWh, $/kW, or both
- Simplify battery replacement cost option so annual replacement can be specified with a single input array instead of two.
- Rename Array DC power variable to Inverter DC input power to avoid confusion about whether value includes power from battery for DC-connected batteries. (`383 <https://github.com/NatLabRockies/SAM/issues/383>`__)
- Fix issue with REopt for PVWatts for rates with no max usage units that caused a user interface error. (`388 <https://github.com/NatLabRockies/SAM/issues/388>`__)
- Fix bug with battery sizing when using AC power values. (`375 <https://github.com/NatLabRockies/SAM/issues/375>`__)
- Update default parameters for battery chemistries. (`323 <https://github.com/NatLabRockies/SAM/issues/323>`__)
- Add a PDF report template for battery configurations. (`302 <https://github.com/NatLabRockies/SAM/issues/302>`__)
- Fix an issue that allowed battery capacity percent to exceed 100% when the calendar degradation model option was set to None. (`478 <https://github.com/NatLabRockies/ssc/issues/478>`__)
- Fix issue with battery thermal model for subhourly simulations. (`472 <https://github.com/NatLabRockies/ssc/issues/472>`__)
- Fix an issue that prevented battery ancillary losses from being correctly applied. (`379 <https://github.com/NatLabRockies/ssc/issues/379>`__)
- Fix issue with battery current and state of charge calculation. (`445 <https://github.com/NatLabRockies/ssc/issues/445>`__)

Retail Electricity Rates and Bill Calculations
----------------------------------------------

- Fix issues with display of electricity bill data for demand charges, net billing credits, and net metering credits. (`442 <https://github.com/NatLabRockies/SAM/issues/442>`__, `372 <https://github.com/NatLabRockies/SAM/issues/372>`__)
- Update electricity rate downloads from Utility Rate Database to use latest API versions, and fix broken downloads due to using http instead of https. (`361 <https://github.com/NatLabRockies/SAM/issues/361>`__, `379 <https://github.com/NatLabRockies/SAM/issues/379>`__)
- Remove 500 rate limit for that caused some rates to be missing for utilties with more than 500 rates. (`361 <https://github.com/NatLabRockies/SAM/issues/361>`__)
- Fix display of net billing dollar amounts in monthly results table. Bill calculations were correct, but table was not displaying correct values. (`372 <https://github.com/NatLabRockies/SAM/issues/372>`__)
- Add option to choose month for end of net metering true-up period. (`16 <https://github.com/NatLabRockies/SAM/issues/16>`__)
- Apply net excess generation credit at end of true-up period to January when true-up period ends in December. (`16 <https://github.com/NatLabRockies/SAM/issues/16>`__)
- Improve metering option variable labels.

Financial Models
----------------

- Improve reporting of energy quantities in PPA Single Owner project cash flow to separate gross and net annual electricity generation so the effect of self-consumption for inverter nighttime consumption, battery charging, and concentrating solar power parasitic loads is clear. (`370 <https://github.com/NatLabRockies/SAM/issues/370>`__)
- Fix labels for incentives inputs so they are visible in the inputs browser and other variable lists to facilitate identifying variables for SDK/PySAM programmers. (`403 <https://github.com/NatLabRockies/SAM/issues/403>`__)

.. _marine-energy-1:

Marine Energy
-------------

- Add graphs to Summary tab on Results page. (`390 <https://github.com/NatLabRockies/SAM/issues/390>`__)
- Add detailed cost break down option for specifying Wave and Tidal energy capital and O&M costs. (`389 <https://github.com/NatLabRockies/SAM/issues/389>`__)

Wind Power
----------

- Implement NREL LandBOSSE wind balance-of-system (BOS) cost model as an option for calculating the BOS capital cost on the System Costs page for the Wind Power model. (`270 <https://github.com/NatLabRockies/SAM/issues/270>`__)
- Fix issue with wind resource data downloads from WIND Toolkit API. (`353 <https://github.com/NatLabRockies/SAM/issues/353>`__, `216 <https://github.com/NatLabRockies/SAM/issues/216>`__)

Concentrating Solar Power (CSP)
-------------------------------

- For molten salt power tower model, replace power cycle Supercritical Carbon Dioxide (sCO2) option with a macro and external Python script, see Power Cycle input page and Help for details. (`454 <https://github.com/NatLabRockies/ssc/issues/454>`__)
- Remove Dish Stirling and Direct Steam Power Tower models from SAM. These models are available in the SAM 2020.2.29 legacy version, which is available for download from the `SAM website <https://sam.nrel.gov/download>`__. (`363 <https://github.com/NatLabRockies/SAM/issues/363>`__)
- For molten salt power tower and physical trough models, display better information about air-cooled condenser model paramters. (`366 <https://github.com/NatLabRockies/SAM/issues/366>`__)
- Remove custom HTF option from Empirical Trough model to avoid errors. (`316 <https://github.com/NatLabRockies/SAM/issues/316>`__)
- Fix issue for the Linear Fresnel Molten Salt model with the solar field option not correctly enabling and disabling the solar multiple and field area inputs. (`201 <https://github.com/NatLabRockies/SAM/issues/201>`__)
- Fix a bug in the Generic CSP model that allowed the cycle to consume more power than available in TES when the solar field output was low. (`477 <https://github.com/NatLabRockies/ssc/issues/477>`__)
- Fix a bug with the Generic CSP model that ignored solar field area at design, and affected the calculation of system power generated. (`476 <https://github.com/NatLabRockies/ssc/issues/476>`__)

Solar Resource Data
-------------------

- Update weather file downloads to work with latest NSRDB APIs so SAM can download from the new 5-minute datasets and data for Europe, Africa, and Asia from the new MSG-IODC datasets, and typical year data as TMY, TGY, or TDY from the new TMY datasets. (`271 <https://github.com/NatLabRockies/SAM/issues/271>`__)
- Fix problems reading weather files from some sources that are based on minute values of 60 for the top of the hour instead of zero. (`365 <https://github.com/NatLabRockies/SAM/issues/365>`__)

.. _2020-2-29-r3:

SAM 2020.2.29 r3, SSC 242: July 31, 2020
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is an update to :ref:`SAM 2020.2.29 Revision 2 <2020-2-29>` that fixes the issues listed beow. For detailed descriptions of issues fixed for this update, see `GitHub Issues for SAM 2020.2.29 Patch 3 <https://github.com/NatLabRockies/SAM/milestone/12>`__ and for `SSC 2020 Patch 3 <https://github.com/NatLabRockies/ssc/issues/435>`__.

- Update wind turbine library.
- New marine energy report templates.
- Run LK script from the command line: [path to SAM executable] [sam filename] [path to LK script file], for example ``c:/sam/2020.2.29/x64/sam.exe no_sam_file c:/my-script.lk``.
- Fix weather file downloads from NSRDB and WIND Toolkit.
- Fix Residential / Commercial cash flow "Send to Excel with Equations" Excel workbook, federal and state income tax rates start in wrong year. This issue only affected workbook calculations, not SAM's cash flow calculations.
- Fix electricity bill issue with net metering end-of-year credit for subhourly simulations &em; was being applied in 8760th time step regardless of simulation time step.
- Fix electricity bill time series buy rates so that annual escalation is applied.
- Disable electricity bill time series buy and sell rates for net metering with $ credits.
- Fix issue with default values of array inputs such as time series electricity buy and sell rates that are disabled by default that caused simulation error or crash when input was enabled.
- Remove battery dispatch "copy schedule" button for Merchant Plant financial model because time-of-delivery factors do not apply in this context.
- Fix battery dispatch period numbers greater than 6 with error handling in SSC.
- Fix battery voltage table option issue that resulted in 100% battery roundtrip efficiency regardless of battery use.
- Fix Vanadium Flow battery voltage model issue that allowed it to discharge more than it was charged.
- Fix Detailed Photovoltaic model DC system availability losses being applied when Enable check box is not checked.
- Correct Detailed Photovoltaic model units reported for transformer no load loss, change from kWh to kW.
- Speed up PVWatts V7 self-shading calculations refactoring code.
- Fix PVWatts V5 single time step model.
- Add support for custom time series length to pvsamv1 and pvwattsv7 (in SSC only, not available in SAM user interface). See https://github.com/NatLabRockies/ssc/pull/418.

.. _2020-2-29-r2:

SAM 2020.2.29 r2, SSC 240: May 28, 2020
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is an update to :ref:`SAM 2020.2.29 Revision 1 <2020-2-29-r1>` that fixes the issues listed beow. For detailed descriptions of issues fixed for this update, see `GitHub Issues for SAM 2020.2.29 Patch 2 <https://github.com/NatLabRockies/SAM/issues?q=is%3Aissue+milestone%3A%222020+Patch+2%22>`__.

- General

  - Code generator fixes for PVWatts, MATLAB, and JSON.
  - Fix LK contour() plotting function.
  - Fix Value of RE macro numbers in CSV file output.
  - Fix rendering of user interface elements in inputs browser.
  - Update Combine Cases macro to work properly with models like Detailed PV that run multi-year simulations.
  - Version upgrade script fixes to handle converting variables when opening files created in older versions of SAM.
  - Fix user interface layout issues for Linux and Mac.
  - Fix stochastic simulations for Mac.
  - Fix issue with automatic updates in Windows (update code looks for win32 folder).

- Financial Models

  - Fix issues with reporting of curtailed energy in cash flow.
  - Fix Single Owner Send-to-Excel with Equations workbook.

- Concentrating Solar Power (CSP)

  - Revert default inputs to SAM 2018.11.11 values.
  - Empirical trough model: Remove custom HTF from user interface per `Issue 316 <https://github.com/NatLabRockies/SAM/issues/316>`__.

- Battery Storage

  - Fix issue with changing configuration from PV Battery to Detailed PV using **Change model** command on Case menu.
  - Fix battery degradation issues.
  - Fix issue where battery state of charge appears to drop suddenly for subhourly simulations.
  - Fix battery sizing inputs for behind-the-meter applications.
  - Clean up user interface variables and labels.
  - Fix bank capacity labels in SSC to help differentiate between user sizing and actual size variable names.
  - Change Generic Battery output variable labels to refer to "system" instead of "PV".

- PVWatts

  - Fix issue with monthly energy calculations for subhourly simulations.
  - Fix bifacial model (pvwattsv7 via API only).

- Fuel Cell

  - Fix configuration issue with Fuel Cell / Single Owner.
  - Fix PVWatts issue with monthly energy calculations for subhourly simulations.

- Detailed Photovoltaic

  - Enable self-shading for one-axis tracking with backtracking.
  - Fix issues with daily lifetime loss inputs.
  - Fix issues with losses outputs.
  - Fix issue with "Tilt = Latitude" for negative latitudes (Southern Hemisphere).

- Biopower

  - Remove Biopower / Commercial configuration. Model is designed for power generation projects, not distributed power applications.
  - Remove feedstock download buttons and links that no longer work.

.. _2020-2-29-r1:

SAM 2020.2.29 r1, SSC 238: March 30, 2020
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is an update to :ref:`SAM 2020.2.29 <2020-2-29>` that fixes the issues listed below. For detailed descriptions of issues fixed for this update, see `GitHub Issues for SAM 2020.2.29 Patch 1 <https://github.com/NatLabRockies/SAM/issues?q=is%3Aissue+milestone%3A%222020+Patch+1%22>`__.

- General

  - Revise and update Help system for battery front-of-meter and behind-the-meter dispatch options, and other general revisions.
  - Add Help buttons to all "Edit Data" windows.
  - Inputs browser improvements to handle inf and NaN values.
  - Fix typos in version upgrade script.

- Detailed PV

  - Add user interface check for out-of-range azimuth values.
  - Fix issue with inverter power limiting calculations.

- Battery Storage

  - Fix issue with opening configurations with batteries from older versions of SAM.
  - Fix battery sizing input variables for behind-the-meter configurations.
  - Fix dispatch input variables for front-of-meter configurations.
  - Disable charge from clipped power option for AC-connected batteries.
  - Enable charging from clipped power for default front-of-meter configurations.
  - Improve handling of time-of-delivery (TOD) PPA price multipliers for front-of-meter configurations.
  - Fix issues with automatic sizing and dispatch (REopt) for behind-the-meter configurations.

- Marine Energy

  - Remove obsolete "Marine Hydrokinetic" macro from Generic System configurations.

- Industrial Process Heat (IPH)

  - Add capacity factor metric for industrial process heat (IPH) models to Results page Metrics table on Summary tab.

.. _2020-2-29:

SAM 2020.2.29, SSC 237: February 29, 2020
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version introduces several new models and features in addition to enhancements to existing models:

- New marine energy tidal and wave performance models.
- New fuel cell performance model.
- New merchant plant financial model.
- New critical load and resiliency metrics for Residential and Commercial financial models.
- Update `PVWatts <#pvwatts>`__ from Version 5 to Version 7 with new module parameter values, optional snow model and `other updates <#pvwatts>`__.
- New battery sizing and dispatch optimization option based on NREL's `ReOpt Lite <https://reopt.nrel.gov/tool>`__.
- New models for grid interconnection limits and curtailment.
- New battery storage category to separate battery model configurations from Detailed PV and Generic System configurations.

For details about work done for this release, see `GitHub Issues for SAM 2020.2.29 release <https://github.com/NatLabRockies/SAM/issues?q=is%3Aissue+milestone%3A%222020+Release%22>`__.

.. _general-1:

General
-------

New model configuration selection tree for choosing models when creating a case to make it easier to find models.

Check for Updates now works for both new software versions and revision number changes within a given version.

LK script fixes to ``to_int()`` function.

Fix Combine Cases macro to correctly assign cost to user input in Generic System case.

Discontinue support for 32-bit Windows versions of SAM Simulation Core (SSC).

All models now include a Grid Limits page for interconnection limits and curtailment to model operational limits imposed by grid operators or regulatory institutions. Availability is a separate input to model operational downtime that the system owner controls such as for maintenance.

Weather Data
------------

Change minute time stamp from 0 to 30 and update data to include GHI data for small collection of default weather files that come with SAM.

Fix time stamp issue with weather file converter macro for SolarAnywhere files.

.. _detailed-photovoltaic-1:

Detailed Photovoltaic
---------------------

Daily AC and DC losses by year inputs for simulation over analysis period option automatically sets array length to the product of 365 days and analysis period in years.

Update CEC module and inverter libraries.

Fix tilt = latitude bug.

Fix bug with PDF report for systems with multiple subarrays.

Fix bug with 3D Shade Calculator returning time zone of zero.

Add simulation warning for one-axis tracking when tilt angle is not zero.

Fix problem when weather file contains POA data only.

Fix problems with inverter input voltage calculations and MPPT limit ratings.

Support simulations with custom time periods, available in pvsamv1 via API only.

New transient thermal cell temperature model

.. _2020-2-29-pvwatts:

PVWatts
-------

Update from PVWatts V5 to PVWatts V7:

- Self-shading applies to the fixed open rack and 1-axis tracking array types and accounts for diffuse shading. In Version 5, self-shading only applies to 1-axis tracking.
- Diffuse shading applies to the 1-axis backtracking array type. In Version 5, it only applies to the 1-axis tracking array type.
- Account for incident angle reflection losses in the module cover using the physical model for incident angle modifier to account for reflection losses of both beam and diffuse irradiance.
- Update parameter values for module type (see table below).
- Apply air mass modifier to account for solar spectrum.
- Optional snow loss model, only available when weather file contains snow depth data.
- New custom dispatch option for battery.
- New automatic size and dispatch optimization based on NREL `ReOpt Lite <https://reopt.nrel.gov/tool>`__.

.. list-table:: Changes to PVWatts Module Type Parameters for PVWatts V7 (V5 / V7)
   :header-rows: 1

   * - Module Type
     - Approximate Nominal Efficiency (%)
     - Module Cover
     - Temperature Coefficient of Power (%/°C) 
     - Fill Factor (for self-shading)
   * - Standared (crystalline Silicon)
     - 15 / 17
     - Glass / Anti-reflective
     - -0.47 / -0.37
     - None / 77.1%
   * - Premium (crystalline Silicon)
     - 19 / 20.1
     - Anti-reflective / Anti-reflective
     - -0.35 / -0.35
     - None / 80.1%
   * - Thin film
     - 10 / 15.6
     - Glass / Anti-reflective
     - -0.20 / -0.32
     - None / 70.6%

The following new features in PVWatts V7 are in SAM Simulation Core (SSC) (accessible via the `SAM SDK <https://sam.nrel.gov/sdk>`__) but not in SAM:

- Stow trackers when wind speed exceeds an upper limit (to avoid wind damage).
- Bifacial modules.
- Transformer losses.
- Soiling loss input as a beam irradiance loss.

Add simulation warning for one-axis tracking when tilt angle is not zero.

Fix problem with battery voltage and temperature model units.

Fix bug with battery chemsitry selection, loss inputs, and lifetime simulation.

.. _battery-storage-1:

Battery Storage
---------------

Separate battery storage model configurations from the Detailed PV and Generic System configurations with a new "Battery Storage" category in the performance model list. (Inputs for the simple battery model are still on the PVWatts System Design input page.)

Convert some inputs to drop-down lists to make user interface less cluttered.

.. _marine-energy-2:

Marine Energy
-------------

New performance model for marine wave energy systems.

New performance model for marine tidal energy systems.

.. _fuel-cell-1:

Fuel Cell
---------

New performance model for photovoltaic array with fuel cell and optional battery storage.

CSP General
-----------

Fix issues with numeric solver "monotonic equation solver" used by CSP models.

.. _2020-2-29-mspt:

CSP Molten Salt Power Tower
---------------------------

A `new Rankine cycle air-cooled condenser model <https://doi.org/10.1016/j.enconman.2020.113025>`__ that calculates turbine back pressure as a function of ambient temperature and cycle heat rejection load. The model uses a normalized polynomial function scaled to ambient temperature at design and minimum condenser pressure. This new model may result in lower total annual output compared to SAM 2018.11.11 and older versions of SAM because at full load heat rejection when the ambient dry-bulb temperature is greater than 10°C, the new model calculates a higher condenser pressure. For the default Molten Salt Power Tower case in SAM, this happens for about 60% of the year, resulting in reduction in total annual electricity output of about 2%.

New inputs that don't appear in UI like ``startup_ramp_time`` and ``startup_target_Tdiff``.

Fix bug with display of "Turbine inlet pressure control" list in user interface.

Fix bug with power cycle evaporative cooling option.

Fix bug for power cycle sizes greater than 150 MW when all other inputs are left at their default values.

CSP Physical Trough
-------------------

Reorganize inputs so that sizing inputs are on new System Design page.

Add dispatch optimization algorithm and inputs on new System Control page. Note that the fossil backup option was removed as part of this work

Fix bug in header pipe calculation to conserve mass flow.

Fix bug for power cycle sizes greater than 150 MW when all other inputs are left at their default values.

Fix physical trough HCE pipe roughness calculations.

CSP Linear Fresnel
------------------

Fix annulus gas pressure (torr) variable name for consistency between molten salt and direct steam models.

IPH Parabolic Trough
--------------------

New system control model with dispatch optimization on System Control page.

Wind
----

Add new uncertainty analysis per IEC 61400-15 guidelines.

Add new loss inputs.

Fix bug with icing cutoff use of relative humidity data.

Add wind direction weibull distribution to results.

Add average annual wind speed as a result.

.. _solar-water-heating-1:

Solar Water Heating
-------------------

Custom mains profile input simplified to annual array instead of annual or monthly options.

Biopower
--------

Improve user interface and documentation to clarify that Biopower model requires weather file with GHI data.

Merchant Plant Financial Model
------------------------------

New financial model for power projects that sell power at hourly or subhourly energy market rates with optional revenue from capacity payments and ancillary services.

Power Purchase Agreement (PPA) Financial Models
-----------------------------------------------

*The Power Purchase Agreement (PPA) models include single owner, partnership flip with and without debt, and sale leaseback.*

PPA price input as array of annual values.

Send to Excel with Equations now works for partnership flip and sale leaseback models.

New optional additional revenue from capacity or curtailment payments.

New optional grid interconnection limits and curtailment.

New Revenue input page for Solution Mode and revenue sources from PPA, and optional capacity or curtailment payments.

Time of Delivery (TOD) factors moved to Revenue page.

Cashflow send-to-Excel with equations fixes for third party ownership host-developer and PPA models.

Behind-the-meter Fianancial Models
----------------------------------

*Behind-the-meter financial models include the residential and commercial models.*

Both by and sell rates can be specified as hourly or subhourly values instead of using time-of-use schedules and periods.

New critical load inputs and resiliency metrics for results.

Residential load data not available for Alaska locations for Download Electric Load macro.

New optional grid interconnection limits and curtailment.

Fix metering option variable labels in SSC utilityrate5 modules to match SAM user interface.

Update Value of RE script to work with URDB CSV file.

Third Party Ownership Finanical Models
--------------------------------------

*Third-party ownership models include the Third Party Ownership / Host and Third Party Ownership / Host-Developer models.*

Fix Host NPV in Send to Excel with Equations workbook.

Fix issues with PDF report when developer NPV is positive and host NPV is negative.

New optional grid interconnection limits and curtailment.

SAM 2018.11.11, SSC 198: November 11, 2018
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This new version introduces new models for third party ownership, bifacial photovoltaic modules, multiple maximum power point trackers for photovoltaic systems, and accesses the latest solar resource data (PSM V3) from the National Solar Radiation Database. It also includes enhancements to the battery storage and CSP parabolic trough models.

This is the most recent version to include the Integrated Solar Combined Cycle (ISCC) model. It has been removed from later versions due to incompatibility with updates to components it shares with other CSP models. If you would like to use the ISCC model, please download and install SAM 2018.11.11.

.. _general-2:

General
-------

Update assumptions for default configurations.

Update user documentation.

Fix stochastic simulations running on Mac OS.

Fix issues with time series data viewer in Mac OS.

Update getting started script for India to use PVWatts so system size scales correctly.

Add new parametric import and export functions to LK scripting language.

.. _weather-data-1:

Weather Data
------------

Improve weather file download for models that use solar resource data to access data from the NREL National Solar Radiation Database (NSRDB). SAM downloads typical meteorological year (TMY) files from the Physical Solar Model (PSM) Version 3 dataset by default, and provides options for downloading single year data, multiple files at a time for different locations or for P50/P90 simulations, and for downloading legacy data from the older NSRDB datasets.

Remove out-of-date weather files from default library.

Improve error reporting when weather file has more than one year's worth of data.

Add support for subhourly data in weather files using the EPW format.

Add support for PVsyst ang PVGIS weather data column labels in the SAM CSV format.

Add check for missing values in weather files using the EPW format.

.. _detailed-photovoltaic-2:

Detailed Photovoltaic
---------------------

Add support for systems with multiple power point trackers (MPPT) with a new inverter input variable for number of MPPT inputs on the Inverter input page.

Add option for modeling bifacial modules with new variables on the Module input page.

Add new macro for optimizing subarray layout given area constraints imposed by rooftop size or available land.

Improve system sizing macro.

Improve System Design input page to streamline system sizing work flow.

Add new inverter temperature model.

Remove misleading error message about using the snow model with one-axis tracking.

Add AC capacity factor as an output variable available on the Data Tables tab.

Enable albedo for parametric simulations.

Display bandgap voltage assumptions for single-diode models.

Update CEC module and inverter libraries with CEC data as of November 4, 2018. Remove inverters with bad data from library.

Fixed issue with voltage calculation for subarray mismatch algorithm. `GitHub Issue SSC 191 <https://github.com/NatLabRockies/ssc/issues/191>`__.

Add implementation of Mermoud/Lejeune module model to SSC. This model option is not available in the SAM user interface, but can be accessed via the SSC API using the `SAM Software Development Kit (SDK) <https://sam.nrel.gov/sdk>`__. `GitHub Issue SSC 26 <https://github.com/NatLabRockies/ssc/issues/26>`__.

PVWatts
-------

Disable tilt and azimuth variables as appropriate based on tracking option.

Third Party Ownership Host / Developer
--------------------------------------

This new model for third party ownership of a photovoltaic system on a residential or commercial property is from the developer's perspective to determine the power price the developer charges the host to meet the developer's costs and return requirements.

Rename the Third Party Host model from older versions of SAM. The Host model is from the host's perspective to compare a lease agreement to a power purchase agreement.

Enable load download macros for third party ownership financial models.

Residential and Commercial
--------------------------

Fix production-based incentive and payback calculations in Send-to-Excel with equations. This was an issue in the workbook, not with SAM results.

Electricity Rates
-----------------

Change labels for excess generation options to standard names for net energy metering, net billing, etc.

Fix issue with calculation of credits for excess generation. `GitHub Issue SSC 154 <https://github.com/NatLabRockies/ssc/issues/154>`__

.. _battery-storage-2:

Battery Storage
---------------

Add battery storage to the Generic System model.

For systems with batteries using the PPA Single Owner financial models, enable Electricity Rates input page to account for the cost of charging the battery and supplying inverter night-time load from the grid and add cost of retail electricity purchases to the project cash flow.

Fix issue with initial state of charge (SOC) not being applied correctly.

Include PV DC and AC losses in power values used for front-of-meter and behind-the-meter battery configurations. `GitHub Issue SSC 137 <https://github.com/NatLabRockies/ssc/issues/137>`__

Fix issue with battery replacements. `GitHub Issue SSC 55 <https://github.com/NatLabRockies/ssc/issues/55>`__.

Fix several issues with dispatch, lifetime, power flow calculations.

CSP Physical Trough Model
-------------------------

Header sizing algorithm considers all sections when progressively downsizing the diameters.

Internal energy of the inlet, outlet, and crossover piping (IOCOP) now fully accounted for.

Runner pressure drop calculation accurately counts expansions and contractions.

Calculation of thermal energy storage (TES) and steam generator system (SGS) piping volumes now more precise.

Script templates added for implementing new features.

Runner and header diameter sizing algorithm revamped:

- More robust in border cases.
- Preferentially selects smaller diameter (cheaper) pipes for out-of-range cases.
- Diameter limit less restrictive for hot (costlier) than cold header pipes.

Calculates (and outputs) HTF properties at design conditions in each pipe section:

- Properties include mass flow, velocity, temperature and pressure.
- Model runs until steady-state is achieved.
- Occurs before yearly simulation.
- Default is to perform, but can be skipped.

Added configurable parameters for solar field:

- North/south distance between subfields (northsouth_field_sep)
- Minimum number of runner expansion loops for each constant diameter segment (Min_rnr_xpans)
- Frequency of runner expansion loops (L_rnr_per_xpan)
- Expansion loop lengths (L_xpan_hdr, L_xpan_rnr)
- Maximum number of header diameters (N_max_hdr_diams)
- Location of the first header expansion loop (offset_xpan_hdr)
- Number of collector loops per header expansion loop (N_hdr_per_xpan)

Added configurable parameters for thermal energy storage (TES)/power block (PB):

- Bypass valve operation during field recirculation (has_hot_tank_bypass) can bypass just hot tank and feed into cold tank (=true) or bypass both tanks and rest of TES/PB (=false).
- Minimum HTF temperature that may enter the hot tank (T_tank_hot_inlet_min).
- Length of runner pipe in and around the power block (L_rnr_pb).
- Lengths of the TES/PB piping (sgs_lengths).
- Design-point velocity for sizing the TES/PB pipe diameters (V_tes_des).
- Minor losses of TES/PB pipe fittings (k_tes_loss_coeffs).
- Pressure drop within the steam generator system (DP_SGS).

Separate design velocities added for sizing hot and cold headers.

Custom TES/PB piping pressure drop calculation option based on Darcy-Weisbach and custom minor pressure losses.

Now reporting TES/PB, runner, header, and loop pipe lengths, diameters, wall thicknesses and number of expansion loops. Minus the number of expansion loops, these output values can be modified and read-in as custom values for subsequent model runs.

Configurable interconnects have been added that replace hard coded component sizes and associated losses, at the inlet, outlet, crossover, and connections between solar collector assemblies (SCAs). Utilized using the following component and component assembly (interconnect) input files, with example usage in the script templates:

- physical_trough_interconnect_components.csv
- physical_trough_interconnect_definitions.csv

Collection (field) and generation (TES/PB) loops have been decoupled. This allows for storage tanks in series with the field in direct storage configurations.

Unit tests have been added to continually verify functions perform as designed.

CSP Power Tower
---------------

Improve user-defined power cycle input tables and documentation.

Internal improvements to SolarPILOT calculations.

CSP Dish Stirling
-----------------

Dish Stirling model annual energy increased by 8% due to the new weather file format.

.. _wind-power-1:

Wind Power
----------

Fix issue with WIND Toolkit data time zone. SAM assumes time stamps are in local time, not UTC. Only affects projects with load data or time-depending electricity prices.

Geothermal
----------

Add LCOE optimizer.

Update and fix bugs in code.

Calculate capacity factor from net nameplate capacity instead of gross nameplate capacity.

Generic System
--------------

Add battery storage.

SAM 2017.9.5 r4, SSC 186: March 14, 2018
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This update restores a few items that were omitted from the SAM 2017.9.5 Revision 3 update and addresses one new issue.

- Improve Generic System Power Plant input page to better handle generation profile as input.
- Restore fix for Wind power model wind resource data downloads from WIND Toolkit.
- Restore Molten Salt Power Tower model supercritical carbon dioxide power cycle option.
- Restore 3D Shade Calculator bug fix for issue with objects below active surfaces causing shadows.
- Restore improvements to cash flow send-to-Excel-with-equations handling of tax and other inputs as arrays.

SAM 2017.9.5 r3, SSC 184: February 21, 2018
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.9.5 Revision 2:

- Detailed Photovoltaic:

  - Update CEC inverter library to February 2, 2018 database. We are using a new conversion algorithm to generate the CEC Inverters library that accounts for discrepancies in the database data and removes most inverters with bad data.
  - Fix issue when using GHI and DHI or POA options for irradiance data from weather file.
  - Fix variable labels of temperature correction standoff options in SSC.

- Detailed Photovoltaic and PVWatts:

  - Fix problem with POA irradiance calculations when the time zone is positive and longitude is negative, such as for some South Pacific islands.
  - Fix an issue with 3D Shade Calculator with objects under and active surface appearing to cause shadows.

- Wind Power:

  - Add support for subhourly simulations.
  - Make Wind Resource Histogram macro available only for Wind Power.

- Industrial Process Heat (IPH):

  - Allow IPH Trough with custom HTF properties.
  - Fix issue with IPH Linear Direct Steam's evacuated tube model.

- Solar Resource Data:

  - Change button labels on Location and Resource input page to clarify their functions.
  - Add a check to weather file reader to check for time zone data in header to prevent simulation errors.
  - Fix an issue with the Solar Resource Interpolation macro with "Same as Load" option.

- Electricity Bill Calculations (Residential and Commercial):

  - Report peak demand by TOU period in results tables.
  - Fix bill calculation for monthly excess with $ credits option to correctly account for rollover credits.

- Send-to-Excel with Equations:

  - Clarify handling of income tax rates entered as tables for Residential, Commercial, and Single Owner financial models.
  - Fix PBI and PTC entered as table for Residential and Commercial.

- PDF report generator:

  - Reports work for all performance and financial models.
  - Graphs show y-axis values and tick marks.
  - PPA price for Third-party Ownership displays correctly (was displayed as zero due to conversion to integer).
  - Fix irradiance units for photovoltaic systems.

- Update **Solar Water Heating** Auxiliary Gas Heater macro to use correct units.
- Fix issue with **Power Tower** SolarPilot shading calculations.
- Add support to **Generic System** for subhourly load data.
- Fix an issue with **Excel Exchange** that prevented SAM from communicating with Excel.

SAM 2017.9.5 r2, SSC 180: October 30, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.9.5 Revision 1:

- Fix a problem with menus and other interface elements caused by Microsoft Windows 10 Fall Creators Update.
- Minor revisions to Help system.
- New sCO2 power cycle option for CSP molten salt power tower model.
- Fix wind Off-shore Balance of System cost model issue with turbine diameter and with values shown on input page.
- Change wind farm loss default value for Wind power with LCOE calculator.
- Fix hidden Return on Equity variable on Financial Parameters input page by expanding Return on Equity input variable panel at bottom of page.
- Fix issues with PV report template handling of annual schedules and accounting of excess generation options.

SAM 2017.9.5 r1, SSC 179: September 17, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.9.5:

- Fix an issue for **Concentrating Solar Power** models with subhourly weather data.
- Update **Solar Resource Interpolation macro** to handle Residential and Commercial cases where the electric load is not defined, and check weather file for required time stamp, irradiance, and wind direction data.
- Update the SAM file used by the **PV India wizard** from SAM 2017.1.17 to SAM 2017.9.5.
- Update **Detailed Photovoltaic model** to only show battery efficiency metric on Summary tab of Results page if battery is enabled.
- Fix issues with **report template** for photovoltaic systems, and:

  - Improve handling of annual schedules for PBI, PTC, and rate escalation.
  - Add support for five metering options and time series sell rates
  - Remove SAM logo for SAM open source

- Remove **interpmet** parameter from SAM CSV weather file format. (Was originally intended for internal testing purposes.)
- Fix issue with **Battery Storage** model's checking of constraints.
- Add missing files Windows dlls and Mac SDKtool app to **SAM SDK**.

SAM 2017.9.5 SSC 178: September 5, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Summary: This version is the first to be built from the new `SAM open source <https://sam.nrel.gov/opensource>`__ repositories. It also includes new capabilities for downloading weather files from the National Solar Radiation Database (NSRDB), and a new direct thermal energy storage model for the industrial process heat physical trough model.

.. _general-3:

General
-------

Added ability to download different types of files from National Solar Radiation Database (NSRDB) for SAM's solar models. It is now possible to download individual single-year files, multiple weather files for P50/P90 analysis, and legacy data directly from SAM.

Detailed Photovoltaic Model
---------------------------

Added weather file checking for for missing temperature or wind speed values.

Updated the Inverter Sizing Information macro to give more information, and renamed it to System Sizing Information.

Fixed kWh/kW metric for subhourly simulations.

Fixed units for GHI data in results.

Fixed problems with PDF report for effective tax rate

.. _battery-storage-3:

Battery Storage
---------------

Improved battery sizing workflow and interface.

Comprehensive reworking of flow battery inputs and model.

Added a calendar-life model for lithium-ion batteries, and a tabular calendar life degradation input for other chemistries.

Added an iron-flow battery to the list of default types.

Improved battery dispatch control.

Fixed several problems with battery model combined with PPA financial model:

- Acount for grid power purchases to charge battery.
- Include battery cost in system installation cost for partnership flip models.
- Inconsistencies how SAM reported some time series outputs

Process Heat Parabolic Trough
-----------------------------

Added direct thermal energy storage. The default solar multiple has increased from 1 to 2.5 and the default for the new input hours of thermal storage is 6.

Added an input for the wind speed at which the trough is stowed. The default wind stow speed is 25 m/s.

Changed controller messages from runtime warnings to notices to minimize information displayed after a simulation. Click the Notices button at the top right of the SAM window to see the messages.

Process Heat Linear Direct Steam
--------------------------------

Improved numeric convergence in the annual simulation and results in fewer simulation crashes.

Changed controller messages from runtime warnings to notices to minimize information displayed after a simulation. Click the Notices button at the top right of the SAM window to see the messages.

Molten Salt Power Tower
-----------------------

Modified the structure by which the tower compute module (cmod_tcsmolten_salt) is called, which affects scripting and API functions. Previously-created LK scripts that are executed using the new SAM release may require modification. The changes occurred in the variable table, in which redundant variables were removed, and the only variable inputs that remain are the minimal set that fully define the system geometry. That is, no calculated parameters are passed as inputs under the new structure. This change simplifies scripting to eliminate the need to provide calculated values as inputs.

Changed some messages from runtime warnings to notices to minimize information displayed after a simulation. Click the Notices button at the top right of the SAM window to see the messages.

.. _wind-1:

Wind
----

Significant changes to the way that SAM corrects power curves for air density. SAM was previously using an older correction method that was appropriate for stall-regulated turbines, but the majority of modern turbines are pitch-regulated. These are better represented by a different correction for air density, which is now implemented. This new method will increase energy production values significantly for turbines at high elevation sites. A description of these two air density calculation methods can be found at https://www.scribd.com/document/38818683/PO310-EWEC2010-Presentation.

PPA Financial Models
--------------------

Fixed a problem with saving TOD Factors data to text file.

Reorganized TOD Factors input page to improve usability of time series TOD factor inputs.

For partnership flip with debt model, fixed problem with accounting of Cost of Development fee in cash flow.

Macros and LK Scriptiong
------------------------

Added median() function to the LK scripting language.

Added a macro to interpolate solar resource data from hourly to subhourly time resolution.

Improved PV sizing information macro.

SAM 2017.1.17 r4 SSC 174: June 19, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.1.17 Revision 3:

- Cash flow "Send to Excel with Equations" for PPA Single Owner financial model: Added support for income tax holidays, debt payment holiday and fixed principal declining interest payments, receivable reserves, and battery replacements.
- Changed Send-to-Excel behavior to open a Save As window instead of automatically saving to hidden Applications folder.
- Residential and Commercial: Fixed an issue with the "Monthly total excess credited to next bill in $ at sell rate(s)" option's calculation of the dollar credit and changed labels of monthly credit output variables.
- Detailed Photovoltaic Model: Fixed non-linear self-shading option to work for weather files with no DNI data.
- Battery model (part of Detailed Photovoltaic Model): Fixed issues with the automated grid power target algorithm, power flow calculations, and controller for AC-connected batteries in self consumption mode.
- Process Heat Direct Steam: Fixed an issue that caused the actual number of loops to be incorrect due to a rounding error that caused the solar multiple to be incorrect.
- Process Heat Parabolic Trough: Fixed behavior of "Choose Number of Loops" macro's Cancel button and updated default values.
- Molten Salt Linear Fresnel: Fixed an issue with the collector azimuth angle.
- CSP power cycle models: Fixed an issue that caused system to generate power in standby mode.
- Power Tower models: Fixed issue with dispatch optimization.
- Molten Salt Power Tower: Improved internal calculations to include mass flow rate constraints.
- Code Generator: Added support for Android Studio for developing Android device apps to use the SAM API. This is a new feature still in the testing phase.
- Fixed an issue with software registration on some computers running Mac OS and OS X.
- Fixed an issue with the inputs browser that caused SAM to crash under certain conditions.

SAM 2017.1.17 r3 SSC 173: April 21, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.1.17 Revision 2:

- Residential and Commercial models: Updated API call to OpenEI Utility Rate Database to match changes in the database made after last SAM release that prevented SAM from displaying electricity rates correctly.
- Detailed PV battery: Fixed reporting of grid power results for DC-connected battery.
- Fixed a problem displaying Help topics from the Results page.
- Solar resource data: Removed trailing zeros from latitude and longitude values in two TMY3 weather files (Blanding, UT and Bryce Canyon, UT) that caused erroneous reporting of DNI exceeding maximum limits.

SAM 2017.1.17 r2 SSC 172: March 30, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.1.17 Revision 1:

- Wind: Made minor updates to cost calculations in offshore balance-of-system cost model.
- Detailed PV: Fixed issue with loss diagram in SAM 2017.1.17 r1 that caused diagram to fail for the detailed PV model with either no financial model or the LCOE calculator financial mode.
- Detailed PV battery:

  - Fixed an issue with the grid power target automated optimization mode, and add outputs for that mode.
  - Detailed PV battery: Fixed minor problems with initialization of the look-behind peak-shaving automated optimization mode.

- CSP Rankine power cycle: Fixed an issue with startup energy calculation that caused thermal startup energy to be too high under some conditions.
- CSP power tower: Changed dispatch optimization input for maximum limit to use 1e+38 as the maximum value instead of the string "inf" that caused an error.
- Help revisions.

SAM 2017.1.17 r1 SSC 171: March 13, 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes the following issues in SAM 2017.1.17:

- Wind:

  - Disabled wake model input for Weibull resource option, and enabled "Specify desired farm and layout" for Weibull option when wake model is not active.
  - Added new turbines to wind turbine library.

- Solar resource library:

  - Fixed a typo in Abingdon VA TMY3 weather file (was Abington)
  - Solar resource library: Added files for new India locations.

- Detailed PV:

  - Updated CEC inverter library.
  - Detailed PV: Improved calculations for display of I-V curve on Module input page.
  - Detailed PV: Added lifetime AC/DC losses to loss auto-graph, and added a few items in loss diagram and loss auto-graph to show or hide depending on if the model is enabled or not.
  - Detailed PV: Improved solver for PV IEC 61853 module model to minimize situations where solver would fail to calculate coefficient values.

- Detailed PV battery:

  - Improved warnings for DC-connected batteries, and add battery power rating on System Design input page.
  - Fixed Vanadium redox voltage model.
  - Added better predictive and iterative control for DC-connected batteries.
  - Fixed an issue with the PV model that caused a crash under certain conditions when battery model was disabled.
  - Updated calculations for battery with front-of-meter connection.

- CSP power tower:

  - Fixed issue that prevented field optimization from running as part of a simulation and caused an error message about the check_max_flux variable.
  - Fixed issue with TOD factors and dispatch optimization.

- CSP direct steam models: Added pressurized water properties
- CSP fossil backup: Fixed an issue with power cycle inlet temperature setting that caused fossil backup to fail.
- PVWatts with Third Party Ownership model: Fixed issue with lifetime simulations that prevented simulations from running.
- Residential and commercial: Fixed issue with units in reported power and energy values.
- SDK code generator: Made general improvements.
- Macros: Made "Value of RE" macro available only for residential and commercial models, and revise instructions.
- Help revisions.

SAM 2017.1.17, SSC 170: January 2017
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Summary: This version adds new capabilities to the photovoltaic, battery storage, concentrating solar power, and wind performance models, as well as new models for solar process heat applications. In addition, major updates to the utility bill calculations allow greater flexiblity for correctly capturing the nuances of complex rate structures. The scripting features have also been improved, and streamlined use of the SAM Software Development Kit (SDK) is possible using the new SDK code generator.

Photovoltaics
-------------

Added an inverter model coefficient calculator that directly accepts inverter efficiency test data for inverters not included in the database. Efficiency data at six different power levels (10%, 20%, 30%, 50%, 75% and 100%) and three different voltages (Vmin, Vnom, Vmax) is required as input.

Added a simple transformer loss model using datasheet no-load loss and full-load loss specifications.

Added the ability to automate the 3D shading scene construction and loss calculations using LK scripting.

Added a fixed seasonal tilt input option.

Fixed several bugs and improved usability of the partial obstruction shading model.

Added daily DC and AC lifetime loss inputs.

Added DC and AC curtailment input options.

Greatly improved simulation speed when using automated battery model dispatch.

Improved user interface for self shading inputs.

Reorganized PV model hourly outputs to improve clarity.

Fixed bug with voltage mismatch calculation when using only one subarray.

Fixed bug with degradation calculations when using multiple subarrays.

Updated default PV costs for all markets.

Updated default DC/AC ratio to 1.2.

.. _battery-storage-4:

Battery Storage
---------------

Added option for modeling batteries at the utility scale with manual dispatch.

Improved dispatch controller using an iterative algorithm that checks battery constraints at the end of the dispatch, and redispatches until all constraints are met (minimum or maximum state-of-charge, charging from grid when disallowed, exporting PV to grid when can charge battery, etc).

Added options to model DC-connected battery system topologies.

Added a Vanadium Redox Flow battery voltage model and default parameters.

Added a simplified battery model option to PVWatts with automatic peak shaving dispatch.

Added AC and DC battery and controller losses to the system loss diagram.

.. _pvwatts-1:

PVWatts
-------

Added a simplified battery model option to PVWatts with automatic peak shaving dispatch.

Updated default PV costs for all markets.

Updated default DC/AC ratio to 1.2.

.. _concentrating-solar-power-csp-1:

Concentrating Solar Power (CSP)
-------------------------------

Added new solar process heat models using a trough or steam linear fresnel solar collectors.

.. _molten-salt-power-tower-1:

Molten Salt Power Tower
-----------------------

Implemented a number of enhancements to the dispatch optimization algorithm, including:

- Cost penalties for receiver and power cycle startup
- A cost penalty for varying electric power output from one time step to the next
- User-specification of the time-discounting factor that weights production as a function of the time step
- Implementation of a maximum net power output to the grid constraint which limits the net production - including parasitic consumption - to a fixed value, if desired.

Implementation of availablity schedules for solar field production

Improvement to solar field optimization to allow for explicit consideration of the maximum flux constraint. Using the algorithm 'BOBYQA', the flux is now directly limited via a linearized constraint, rather than included in the optimization objective function via a penalty factor. This improves the reliability of the solar field optimization algorithm. Note that other algorithm options have been removed, along with the flux over-design penalty input.

Default values for the heliostat field, receiver, and tower have been updated

Direct Steam Power Tower
------------------------

See "Molten Salt Power Tower" notes regarding solar field optimization.

Default values for the heliostat field, receiver, and tower have been updated

Direct Steam Linear Fresnel
---------------------------

Fixed the Evacuated Receiver field modeling option to use the correct assumptions implied by the "Broken Glass" checkbox. Bug in previous model resulted in simulation of the opposite case.

Fixed the IAM calculation used to model the collector/receiver optical performance. Bug in previous version used the traverse incidence angle in both the Traverse and Longitudinal IAM polynomials.

CSP generic model
-----------------

Added solar field availability and curtailment losses dialog to allow constant, time-period, and hourly specification

Updated default values

The Generic CSP System model was restructured and now uses different regression equations for thermal losses, power block conversion efficiency, and parasitic consumption. This change invalidates previous model configurations and requires revised input values. For more information on generating regression coefficients, view help or visit the SAM website.

Added DNI adjustment for parasitic loads

Added exergy correction model for thermal storage charge state

.. _solar-water-heating-2:

Solar Water Heating
-------------------

Removed redundant output variables to improve clarity.

Updated units of some output variables for improved consistency.

.. _wind-2:

Wind
----

Added nearly 200 wind turbine power curves.

Fixed bug with how cut-in wind speed was calculated for turbines in the library.

Fixed bug with the Weibull resource plot on the wind resource input page.

Added a sales tax taxable basis input to the wind cost page.

Updated default costs for all markets.

Updated default turbine selection for some markets.

Added wind turbine power curve as a model output.

Fixed bug to correctly apply availability and curtailment losses when using the Weibull distribution resource model.

Added offshore balance of system cost model.

Added wind resource data file range checks on data values.

.. _ppa-financial-models-1:

PPA Financial Models
--------------------

Added ability to handle negative inflation and real discount rates.

Added state and federal tax yearly tax holidays.

Added fixed principal declining interest loan payback option.

Added fractional month calculations for all reserve accounts.

Added receivables reserve account bases on ppa revenue.

Added loan moratorium to debt fraction loan repayment option.

Improved the organization of cash flow line items in the results.

Added return on equity (ROE) input for modeling specific financing scenarios in international locations.

Residential and Commercial Financial Models
-------------------------------------------

Added new 'Value of RE' macro to model utility rate switching.

Added ability to handle negative inflation and real discount rates.

Added state and federal tax yearly tax holidays.

LCOE Calculator
---------------

Minor update to the financial input page to improve clarity.

Updated all default parameters to be consistent with the NREL Annual Technology Baseline report.

Utility Rates
-------------

Improved options for monthly accounting of excess generation.

Updated monthly outputs to include year end dollar rollovers.

Added additional monthly outputs to balance monthly bill with energy charges and credits.

Added ability to handle negative inflation and real discount rates.

Added option to specify hourly or subhourly sell rates.

Scripting
---------

Fixed a bug in that caused occasional crashes when assigning a value to a variable that was stored on the right hand side as an element in a complex data structure having the same variable name. e.g. ``x = x[1];``

Added the SDK Code Generator that will create C/C++, Java, MATLAB, PHP 5/7, VBA, C#, or Python 2/3 template code files for a SAM case.

New script-based plotting features for filled or unfilled 2D contour plots, reversed axes, sector plots, color bars, GIF animations, and graphic and textual annotations.

Fixed issue with setting working folder when running scripts through the LK debugger.

Allow option to break into LK debugger if a script error occurs.

Revised curl() function for more control of processing http/https requests.

Added JSON read/write functions.

Fixed bug in switch statment ``( ? x [ 1, 2, 3 ] )`` processing.

Allow lazy syntax in table initialization, e.g. ``x = { a=1, b='text', c=14.9 };``

Updated dview() function syntax to remove scaling factor option and added option to indicate which dataset should be selected in the viewer by default.

Added new progressbar() function.

Fixed bug in stable_sort() function, and added optional parameter for column sorting.

Added macrocall() function to allow calling SAM Macros from within a script.

.. _general-4:

General
-------

Display rendering improvements for high-DPI screens.

Fixed bug in P50/P90 simulation for user-specified P-values.

Fixed a bug in the 'Combine Cases' macro.

SAM 2016.3.14 r4, SSC 160: September 2016
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes an NSRDB download issue in SAM 2016.3.14 Revision 3:

- Fix an issue with downloading solar resource data from the NREL NSRDB. (This issue was introduced in SAM 2016.3.14 Revision 3.)

SAM 2016.3.14 r3, SSC 160: September 2016
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update with one new feature and that fixes several issues:

- New code generator converts SAM inputs into LK, MATLAB, Python, Java, PHP, C#, or VBA code for use with the SAM SDK.
- PDF report template: Fixes for subhourly load data (PV template) and large nameplate capacities (basic template).
- Detailed PV with battery: Remove calculated user-interface variables from parametric inputs list.
- Detailed PV: Report string Voc and Isc in time series results.
- Detailed PV: Add error reporting to avoid errors in inverter clipping calculation when mismatch is enabled with one subarray.

SAM 2016.3.14 r2, SSC 159: July 2016
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes several issues:

- Updated options for P50/P90 analysis
- Fixed timestamp in Copy-to-clipboard in results
- Improved sizing messages for module and inverter selection
- Fixed bug in error messages on wind turbine design input page
- Fixed typo on IEC61853 input page for temperature units
- Fixed several cut-and-paste issues for data browsers
- Fixed bug with 3D shading tool group names
- Fixed bug in tabular browser
- Fixed bugs with utility rates in PV report
- Fixed bug in PV model when running shade database model with POA input data
- Fixed bug in PV model when running POA model with multiple sub arrays
- Fixes to Java wrapper in SDK
- Fixed crash for financial models with analysis periods less than 11 years
- Fixed bug with energy charge and demand charges for two meter configurations
- Fixed bug in PV model snow loss calculation
- Fixed bug with valid irradiance thresholds in PV model
- Fixed crash in CSP dispatch optimization when using user defined power cycle
- Fixed crash in CDF viewer in results

SAM 2016.3.14 r1, SSC 159: April 2016
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes several issues:

- Fix for downloading solar resource data files from the NREL NSRDB
- Demand charge tiers are now correctly applied
- Escalation factor correctly handled in utility rates
- Fixed issue with kWh/kW units in utility rates
- Updated CEC module database
- Fix for importing SunEye shading data
- Fix for table formatting in shading azimuth/altitude input
- Fixed problem in CSP tower model with short power block startup times
- Fixes in the shading input user interface
- Fixes in the PV report template
- Updated physical trough receiver library
- Updates to help documentation

SAM 2016.3.14, SSC 159: March 2016
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version adds new capabilities to the photovoltaic, battery storage, and molten salt power tower performance models with a new model for integrated solar combined cycle (ISCC) concentrating solar power plants. This version also adds new electricity metering options and other electricity rate improvements to the residential and commercial financial models. It includes a new data browser for tabular results and many enhancements to the LK scripting language.

.. _photovoltaics-1:

Photovoltaics
-------------

- New option to use plane-of-array (POA) irradiance data in the weather file as input, works with data from a pyranometer (broadband) or reference cell
- New obstruction shading model calculates beam shading factors for a subarray represented in the 3D shading calculator as consisting of up to 8 parallel strings
- New self-shading option for subarrays that experience linear reduction in output with an increase in self shading
- New capability for the module and inverter models with user entered parameters to save and load parameters from a text file
- Limit albedo values to greater than zero and less than one (non-inclusive). Albedo = 0 and albedo = 1 are invalid (non-physical)
- Change results variable names for irradiance values to make it clearer which values are from weather file and which values are calculated by transposition model, e.g., change Beam irradiance to Irradiance DNI from weather file
- Added warnings to let the user know if SAM calculates a negative irradiance value and sets it to zero
- Fixed bugs in the Append Snow Data macro
- Bug fix to allow self-shading to run even if the weather file does not contain DNI data
- Bug fix in the existing non-linear self-shading algorithm that may have been slightly underestimating shade losses
- Bug fix for self-shading outputs in lifetime mode
- Improved feedback when setting up a self-shading scenario to make sure the self-shading is consistent with the system design
- Additional detail in the module and inverter libraries to make it easier to sort equipment
- Removed redundant total DC and total AC loss inputs
- Bug fix in the treatment of AC wiring and transformer losses at nighttime: they were incorrectly lessening the power draw at night, now they correctly add to it
- Update report template to work with new electricity rate inputs and results and fix bugs

.. _battery-storage-5:

Battery Storage
---------------

- New automated peak-shaving mode for demand charge reduction
- New power target mode
- New ability to schedule weekend dispatch separately
- New ability to prioritize photovoltaic power to battery or electric load
- New ability to customize grid charging levels
- Additional outputs to view battery metrics

.. _pvwatts-2:

PVWatts
-------

- Uses hourly or subhourly shading losses from the 3D shading calculator rather than month by hour values.

Concentrating Solar Power (CSP) Models
--------------------------------------

- Water usage reported for all CSP models
- New integrated solar combined cycle (ISCC) performance model

Parabolic Trough (Physical)
---------------------------

- New user-defined power cycle option accepts performance data from an external power cycle model to define the performance characteristics of Rankine or other types of cycles.
- This release fixes a bug that resulted in unreasonable freeze protection parasitics. This bug typically occurred during subhourly simulations, however the fix may also improve the annual energy for some hourly simulations

Parabolic Trough (Empirical)
----------------------------

- This release fixes a bug in the implementation of the Cooling Tower Correction defined on the Power Block page. The previous version applied a wet bulb correction when Dry-bulb Basis was selected for the Temp. Correction Mode, and it cancelled the temperature correction when Wet-bulb Basis was selected (i.e. the performance model did not adjust for temperature variation during the annual simulation). This fix did not affect the 6.30.2015 default case, as the Cooling Tower Correction was set to a constant value (i.e. F0 = 1 and the remaining coefficients were zero). However, the bug could have a sizable effect (5-20%) on cases expecting a temperature correction
- Fix a bug that prevented Power Block parameters from the library to be applied when you selected an item in the library

.. _molten-salt-power-tower-2:

Molten Salt Power Tower
-----------------------

- New dispatch optimization model and plant control algorithm with numerical solver automatically optimizes the dispatch of the thermal energy storage system to maximize plant revenue based on the time-of-use period PPA price multipliers defined on the Time of Delivery Factors page. If dispatch optimization is not selected, the plant control algorithm determines how to operate the power tower system based on the time-of-use periods and Turbine output fraction schedule defined on the new System Control input page.
- New user-defined power cycle option accepts performance data from an external power cycle model to define the performance characteristics of Rankine or other types of cycles.
- New Matrix Data results for receiver flux map, solar field optical efficiencies, and heliostat positions
- Reorganize user inputs: New System Design page has inputs that define the system nameplate capacities and design point parameters, System Control page for the new dispatch optimization inputs and other variables related to the operation of the system, Thermal Storage page now only has inputs to define the capacity of the TES, Parasitics page removed and those inputs moved to input page for relevant component, e.g., power cycle HTF pump power is now on Power Cycle page instead of Parasitics page.
- Supercritical carbon dioxide power cycle, thermocline storage, and fossil backup are not available in this version of the power tower model because of the new system control and optimization models.

Molten Salt Linear Fresnel
--------------------------

- New user-defined power cycle option accepts performance data from an external power cycle model to define the performance characteristics of Rankine or other types of cycles.

.. _direct-steam-power-tower-1:

Direct Steam Power Tower
------------------------

- This release correctly calculates heliostat tracking parasitics. The old version was underreporting this value. This fix reduces the annual energy output by about 0.5% in the default case.

.. _wind-3:

Wind
----

- Added step-by-step wizard to help introduce new users to the wind model
- Updated the default assumptions for the Define turbine design characteristics blade and tower design options to reflect current technology

.. _solar-water-heating-3:

Solar Water Heating
-------------------

- Uses hourly or subhourly shading losses from the 3D shading calculator rather than month by hour values.

.. _ppa-financial-models-2:

PPA Financial Models
--------------------

- Allow PPA price escalation rate to be negative and greater than 100%
- Fix bug in PPA partnership flip models that caused ITC to be incorrectly allocated to the partners
- Fix bug in PPA partnership flip models that caused flip year to be incorrectly determined due to invalid IRR values being reported as NaN instead of zero
- Fix bug with how salvage value is accounted for in the cash flow for PPA financial models
- Minor revisions of output variable labels

.. _residential-and-commercial-financial-models-1:

Residential and Commercial Financial Models
-------------------------------------------

- Minor bug fix in the Download Electric Load macro to handle changes in the OpenEI data format

.. _utility-rates-1:

Utility Rates
-------------

- New electricity metering options to handle a broader range of electricity rate structures, including net metering and feed-in tariffs
- New maximum usage units for tiers in addition to kWh: kWh/kW, kWh/daily, kWh/kW daily
- Replace individual inputs and results for energy and demand rate structures with tables to simplify editing and analyzing rate structure data (this version converts data from old .sam files into new format)
- Revise labels of results variables and add new kWh export variable
- For electricity rates that combine net metering with time-of-use periods and tiers pro-rate rollover credits to next month's time-of-use periods
- For electricity rates with net metering and time-of-use periods, apply method for rolling over credits to appropriate periods when the next month has a different time-of-use schedule (assume periods at 12 am, 6 am, 12 pm and 6 pm determine periods to roll over to)
- Flat rates now modeled as energy charges under a single tier and time of use period
- update to new OpenEI Utility Rate Database

.. _general-5:

General
-------

- SAM uses the new NREL National Solar Radiation Database (NSRDB) for satellite irradiance data on a 4 km grid (see `http://nsrdb.nrel.gov <http://nsrdb.nrel.gov/>`__).
- Improved results browser can display matrix data and groups results by category
- Updated default values to reflect current market conditions, especially in PV and wind
- New feature to import one or more cases from a .sam file into the currently open file
- Updated output variable labels for clarity
- Internet access proxies are automatically detected on Windows
- User interface scales properly on high DPI laptop and tablet screens on Windows and OSX
- Scripting language has a new interactive debugger that lets you step through your script line by line
- Scripts run significantly faster due to new virtual machine architecture
- Several built-in script functions added or improved (csvread, csvconv, curl, stable_sort, choose_from_list, transp, tempfile)
- Bug fixes in Combine Cases macro to work with various financial models
- Real-time syntax checking in script editor does not hang the user interface when editing long scripts
- Improved rendering for plots when exported in PDF format
- Bug fixes for horizontal stacked bar plots
- Bug fix for time stamp shown in results viewer for 1 minute time series data
- Updated weather file converter script to handle SolarAnywhere data files
- Bug fix for saving projects with different 3D shading scenes in multiple cases
- Linux version updated to remove webkitgtk dependency
- P50/P90 functionality updated to run correctly if file names contain periods

SAM 2015.6.30 r4, SSC 47: October 2015
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes several issues:

- Added access to some international utility tariffs in IURDB
- Fix for Send-to-Excel export with operation and maintenance variable costs
- Improvements to PV report template
- Bug fix for net metering calculations with energy rollover
- Initialize battery with maximum SOC at start of simulation
- Added number of battery cycles to output
- Updates to help documentation

Revision 4 replaces Revision 3, which had a display issue on some Windows computers.

SAM 2015.6.30 r2, SSC 47: August 2015
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes several issues:

- Improved behavior of battery dispatch controller when grid charging and discharging are both selected.
- Fix for single point efficiency module model MPP and OC voltages in simulation.
- Fixes for 3D diffuse shading calculation.
- Fix error when using electric load data with the Lifetime simulation option in the PV model.
- Fix in time series viewer for drawing long time series (OSX and Linux).
- Fix for default axis scaling for stacked bars, and incorrect stacking of horizontal bar plots.
- Added option to enter weather data to pvsamv1/pvwattsv5 as arrays rather than reading file from disk.
- Fix for residential/commercial Send-to-Excel with operation and maintenance costs as annual schedules.
- Fix for single owner financial model Send-to-Excel with operation and maintenance costs as annual schedules.
- Flat rate inputs now available for parametrics.
- Clean up of unused input variables in SSC direct steam power tower model.
- Fix parametric number of runs to make it editable.

SAM 2015.6.30 r1, SSC 46: July 2015
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update that fixes several issues:

- Update CEC module and inverter databases.
- Fixed bug in battery storage model that prevented out years replacements for being handled properly.
- Fixed bug in solar water heating system collector efficiency calculations.
- Revised treatment of salvage value for utility-scale financial models.
- Detailed PV model clips array voltage to specified inverter MPPT tracker voltage range.
- Fixed bug in PVWatts and IEC 61853 module model that sometimes
  overpredicted energy production in high latitude locations with very
  high tilt angles.
- Enabled reading TMY3 formatted files in which the hour data column is offset by one.

SAM 2015.6.30, SSC 45: June 2015
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a major release that introduces several new models:

- PV with battery energy storage, including simulation of the whole system lifetime.
- Third party lease or PPA financing for distributed systems.
- Simple LCOE calculator based on a fixed charge rate.

.. _photovoltaics-2:

Photovoltaics
-------------

- A new battery model for lead-acid and lithium-ion batteries in grid-connected photovoltaic systems with storage. The model accounts for storage capacity, terminal voltage, temperature, and capacity fade.
- A detailed manual dispatch controller allows custom tailoring of how battery energy is discharged to meet a building or facility's electric load. We hope to add automated and optimized dispatch controller strategies in future versions of the model.
- A new lifetime analysis mode makes it possible to run year-by-year simulations over the analysis period at hourly or sub-hourly (down to one minute) time-steps. This mode is particularly important when analyzing battery systems to properly account for the effect of cycling on battery life and battery replacement costs.
- A battery macro which allows quick financial comparison of lifetime costs between a PV system with batteries, a PV system without batteries, and no PV system.
- New options to apply degradation to the array's DC output and run simulation over the entire analysis period.
- New input for DC power optimizer loss, associated new buttons to overwrite defaults with appropriate losses for central inverters, dc optimizers, and microinverters.
- Performance ratio bug fix- performance ratio was being incorrectly calculated using post-shading and post-soiling irradiance. Updated to use pre-shading and pre-soiling irradiance. This will decrease performance ratios slightly in cases where shading or soiling are present.
- Added model for predicting energy lost due to snow fall on a PV system. Requires snow depth data in the weather file.
- Bug fixes with SunEye and Solar Pathfinder file imports.
- Bug fixes in inverter sizing macro.

Power Tower (Molten Salt)
-------------------------

- Under some conditions, the model was not tracking tower/receiver startup parasitics. We have fixed this in the current release. The changes result in less than 1% decrease in annual energy production for the default case. The changes may be slightly more significant for less sunny locations.

.. _wind-4:

Wind
----

- Bug fix in cost and scaling model for the power curve of user defined turbines. Will change the answers for user-defined turbines.
- Bug fix- was skipping negative x values for wind turbine layout import button.

.. _ppa-financial-models-3:

PPA Financial Models
--------------------

- Improved metrics describing project term debt to make clear the relationship between size of debt and net capital cost.
- Enabled single owner cash flow send-to-Excel with equations for debt percentage debt sizing option.
- Improved cash flow to show net capital cost calculation.

.. _residential-and-commercial-financial-models-2:

Residential and Commercial Financial Models
-------------------------------------------

- Revised net metering calculations: One bi-directional meter where
  sell rates are equal to buy rates except for monthly energy rollover
  with year end sell rate. One can expect the renewable system to offset
  peak demand and energy charges.
- Revised non-net metering (gross feed in tariff) calculations:
  Two uni-directional meters: a) system to grid where utility buys
  renewable system energy at specified sell rate, b) grid to load where
  property owner buys electricity at the specified buy rate. One can
  expect system specified outputs to be the sum of the two meters and that
  the renewable system will not offset peak demand or energy charges but
  will be compensated at the corresponding sell rate.

New Financial Models
--------------------

- Third Party Ownership model for a photovoltaic system installed on
  residential and commercial property with a third party that owns the
  system and sells power to the residential or commercial customer under a
  lease or power purchase agreement.
- LCOE Calculator uses a simple fixed charge rate method to
  calculate the levelized cost of energy for market-level or very
  preliminary project analysis.

Miscellaneous
-------------

- Improved rendering speed of time series data viewer for high temporal resolution simulation results.
- Added searchable script library function help system.
- Improved syntax error reporting in script editor.
- Updated scripting language reference manual.
- Improved search and filter for graph editing and data tables in the results viewer.
- Bug fixes in several built-in script library functions.
- If a leap day is included in the weather file, it will be
  automatically skipped for purposes of annual simulation assuming 8760
  hours in a year.
- SAM can now read TYA weather files (TMY3 update).
- Fixed typo in name of Santa Fe weather file.
- Updated PPA price defaults for all technologies.
- Renamed adjust.factor to adjust.constant to make it consistent with stored values

SAM 2015.1.30 r1, SSC 41: March 2015
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to SAM 2015.1.30 that fixes critical
issues with the Molten Salt Linear Fresnel and Physical Trough models
and several other smaller bugs as listed below.

.. _general-6:

General
-------

- Save files from the Electricity Rates input page uses the Save dialog instead of Open to resolve issue in OS X.
- Stochastic simulations work in OS X.
- SAM runs on Windows Vista.
- Registration window can be resized on computers with small screens to make OK and Cancel buttons visible.
- Running a simulation with a custom weather file that has no data generates a simulation message.
- Download Electric Load macro does not limit number of downloads.
- Santa Fe, NM appears in solar resource library search results (fixed a typo in the location name).
- Excel Exchange works with .xlsm files.

.. _detailed-photovoltaic-model-1:

Detailed Photovoltaic Model
---------------------------

- New CEC inverter and module libraries.
- PDF report template displays parameters for modules with
  user-defined specifications, radiation values for weather files with no
  GHI data, and Vmp value.
- System Design page displays a message for Specify System Size option
  with inverters in the library that have no voltage limits to indicate
  that auto-sizing algorithm will set modules per string to one.
- Inverter sizing macro works for systems without a financial model and reports inverter clipping losses in correct units.

.. _residential-and-commercial-financial-models-3:

Residential and Commercial Financial Models
-------------------------------------------

- Year one sales and purchases value includes demand charges.
- Send-to-Excel workbook calculates correct Property Assessed Value.
  This issue did not affect the cash flow in the workbook or SAM results.

Concentrating Solar Power Models
--------------------------------

- Models work with custom solar field and thermal energy storage heat
  transfer fluids. (The models will calculate incorrect results with a
  custom fluid for both the field and storage. This issue will be resolved
  in the next version of SAM.)
- Molten Salt Linear Fresnel correctly calculates thermal energy storage tank diameter.
- Molten Salt Linear Fresnel applies correct solar field inlet temperature and calculates freeze protection energy.
- Physcial Trough receiver library entry for Royal Tech has correct Variation 1 Annulus Pressure value.

Generic System Model
--------------------

- Make cost input variables available for parametric simulations, and group them properly in variable lists.

.. _wind-5:

Wind
----

- Correct errors in the annual energy calculation for Weibull speed distributions with both user-defined and library turbines.
- Wind resource file chooser displays only \*.srw files instead of solar resource files.

SAM 2015.1.30, SSC 41: January 2015
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to SAM 2014.11.24 that addresses several important bugs and adds new features:

- Fixed bugs in the PPA financial model calculations of levelized cost and levelized PPA price metrics
- Added an option to the PPA financial models with debt (Single Owner and Leveraged Partnership Flip) that makes it
  possible to specify the size of debt as a percent of installed cost with fixed annual payments in addition to the current
  option to size debt with a debt fraction based on cash available for debt service
- Added a Combine Cases macro to model a project that combines a single financial model with two or more performance models
- For the residential and commercial financial model electric load input pages, added an option to scale hourly data to
  monthly utility bill data
- Added support for subhourly simulations for the physical trough model
- Fixed a file association issue that caused SAM to associate the old .zsam file name extension with SAM
- Updated the CEC inverter and physical trough collector libraries
- Updated default values for cost and financial parameters

.. _general-7:

General
-------

Updated user documentation

Updated default PPA prices, internal rate of return, and real discount rate inputs to be more representative of current
market conditions

Removed duplicate annual energy outputs

Fixed bug in which cash flow display was not displaying all years of analysis period

Fixed bug with rendering of macro list when changing models

Updated cashflow send-to-excel to handle fuel costs and TOD price factors

Changed availability and curtailment inputs from factors with values between 0 and 1 to losses with values between 0 and
100%

Added a search box to results data table list to filter variables by name

Fixed a problem with rendering of unicode characters in PDF reports in OS X

.. _residential-and-commercial-financial-models-4:

Residential and Commercial Financial Models
-------------------------------------------

Electricity rate search box can now resolve duplicate names for the same utility

Electricity rate lookup by zip code

Electricity rates with flat buy and sell rates without net metering now trued up on an hourly basis instead of
monthly basis

Updated diurnal demand charge TOU schedule requirement for rates that have only a flat monthly demand charge

.. _ppa-financial-models-4:

PPA Financial Models
--------------------

In specify target IRR mode, changed initial guess in PPA price search algorithm to 0 instead of using value of disabled PPA
price input

Fixed recapitalization bug

Improved reporting of invalide results

Fixed algorithm to correctly handle a target IRR input value of zero

.. _detailed-photovoltaic-3:

Detailed Photovoltaic
---------------------

Bug fix in azimuth x altitude shading table input

Fixed bugs with module I-V plots on the CEC Performance model with user entered specifications

Fixed unit conversion bug for temperature coefficients in CEC Performance model with User Entered Specifications

Total module area on System Design page now updates appropriately

GCR limit increased to 3 to be consistent with PVWatts

.. _concentrating-solar-power-csp-2:

Concentrating Solar Power (CSP)
-------------------------------

Fixed bug in auxiliary gas heater macro

Generic CSP- updated thermal storage to work with dispatch factors

Molten salt linear Fresnel: Added option for electric or fossil storage freeze protection

Fixed bug when thermal energy storage is set to 0

Fixed problems occuring with user-defined heat transfer fluids

Updated Linear Fresnel and CSP Tower cost defaults

Rearranged inputs on Molten Salt Power Tower- Tower and Receiver input page

.. _wind-6:

Wind
----

Updated wind defaults to not include the ITC

LK Scripting
------------

Additional error messages for variable handling in scripts

Added LK functions for running simulations in parallel within a script

Added new stepwise regression and Latin Hypercube Sampling LK script functions

SAM 2014.11.24, SSC 40: November 2014
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a major reworking of the SAM software with a completely rebuilt software structure and user interface. This is the first version of SAM to use the SAM Simulation Core (SSC) framework. It adds
several major new capabilities:

- Completely revamped and significantly simplified user interface
- Updated LCOE and PPA calculations for utility-scale systems
- Simplified financial model options
- Integrated 3D shading calculator for photovoltaic and solar water heating systems
- New CSV-based weather file format and included database of 1600 locations
- Redesigned results browser and parametrics user interface
- Parallel processing for parametric and stochastic simulations
- New scripting engine with a syntax-aware editor and support for user Macros
- Brand new CSP model engine is faster, added a molten salt linear Fresnel CSP model
- Updated to use the latest PVWatts Version 5 algorithms
- Support for the OpenEI Utility Rate Database Version 3
- New building electric load estimator algorithm
- New SAM project file format reduces project sizes (not backwards compatible)

Numerous other features and bug fixes are included. Free online
registration of your copy of SAM is required using your
email address when you first start the software. A registration key
is then emailed to you and used to verify registration each
time you start SAM.

Photovoltaic (detailed)
-----------------------

New 3D shading calculator will estimate linear beam shading factors based on a 3D scene entered by the user.

Added support for subhourly simulations (1, 5, 10, 15, 20, 30 minute, and 1 hour)

Added an inverter sizing macro to give additional information about system sizing by analyzing simulation results.

Self-shading calculator for large arrays is now enabled for all four PV sub-arrays, and inputs are simplified for easier
use.

Reorganized input pages for easier use by separating Array and PV Subarrays pages into System Design, Shading, and Losses
pages

Transitioned from derates to losses to be more intuitive and consistent with industry practice

Self-shading for one-axis tracking systems now uses the same algorithm as self-shading for fixed tilt systems for
consistency and more accurate self-shading calculation including estimated nonlinear losses

Changed the calculation method for auto-sized systems to get closer to the desired DC-AC ratio.

Clipping losses, nighttime tare losses, and inverter power consumption losses now reported correctly. Previous versions
reported losses for one inverter, not the total number of inverters in the system.

Added a $/Wdc option for inverter pricing and changed to this unit by default.

Cost assumptions have been updated to match the latest PV cost data.

Photovoltaic (PVWatts)
----------------------

Revised algorithms according to the PVWatts version 5 update, including an updated inverter performance model, self-shading
or backtracking for 1 axis trackers, module models for standard/premium/thin-film types, updated system loss assumptions.

Added support for subhourly simulations (1, 5, 10, 15, 20, 30 minute, and 1 hour)

Cost assumptions have been updated to match the latest PV cost data.

High Concentration PV
---------------------

The cell efficiency table is now interpolated based on incident DNI, rather than cell POA.

Updated the thermal model to use incident DNI rather than cell POA now consistent with the Sandia model.

.. _wind-7:

Wind
----

Access to NREL's new WIND Toolkit data through the SAM user interface

Auto-layout capability for farms by entering a desired farm size

Turbine spacing in a farm is now entered in units of rotor diameters instead of meters.

Hub height was not previously accounted for in the air density calculations to compute the power curve when inputting
turbine parameters and using a Weibull resource distribution

The air density equation used to compute the power curve when inputting turbine parameters has been updated with more
accurate constants:

Siting considerations page has now become a macro

NREL's new Land-Based Balance of System cost model has been integrated into the user interface

Default turbine spacing assumptions have changed.

Cost assumptions have been updated to match the latest wind cost data.

.. _parabolic-trough-physical-1:

Parabolic Trough (Physical)
---------------------------

Fixed bracket heat loss calculations to match empirical correlations in the Physical Trough Manual. The result is slightly
reduced bracket losses.

Improved the convergence of the first-principles heat loss model.

.. _parabolic-trough-empirical-1:

Parabolic Trough (Empirical)
----------------------------

Fixed a problem with calculation of the time zone

Updated user interface with simplified selection of components from the libraries

Power Tower Technologies (Molten Salt, Direct Steam)
----------------------------------------------------

Replaced heliostat field simulation model to better support data formats provided by SolarPILOT and to automate field
geometry characterization. The new model incorporates layout, flux, and efficiency characterization simulations directly. In
the previous model, heliostat field optical efficiency was evaluated on a regularly spaced azimuthal/elevation sun position
grid, and values were interpolated with a simple bilinear algorithm. The new model evaluates efficiency at the same sun
positions as the flux maps, and efficiency is evaluated using Gauss-Markov estimation, which amounts to linear interpolation
among irregularly spaced data points.

Added the ability to specify the number of flux maps used in field characterization. These settings are available on the
Tower and Receiver technology pages. The user may also specify the resolution of the flux map.

Previously, the receiver flux distribution was determined during
simulation by choosing the pre-generated flux map closest
in sun position to the current point. This was updated to allow
interpolation among several nearby flux maps with a weighting
approach.

The previous DELSOL3-based field layout and characterization algorithm is replaced with NREL's SolarPILOT software. The new
approach emphasizes full layout and position-based simulation rather than DELSOL's zonal approach.

Improved accuracy of heliostat field characterization given x-y position layout. This was poorly supported under DELSOL3,
but is a point of emphasis with the new SolarPILOT algorithms.

Added support for heliostat optical parameters, including heliostat canting (individual mirror facets) in the horizontal and
vertical direction, ideal vs. flat (no) focusing, and options for specifying facet aiming (canting).

Updated heliostat field optimization algorithms. Please note that documentation is still under development for these
algorithms. The user may refer to
http://ab-initio.mit.edu/wiki/index.php/NLopt_Algorithms
for an overview of the methods.

.. _power-tower-molten-salt-1:

Power Tower (Molten Salt)
-------------------------

Support for automatic re-layout of the heliostat field in parametric or stochastic simulations

Removed cavity receiver option until model can be integrated with new system level model

Fixed an error that occurred when the number of receiver panels was less than the number of flux points.

Updated the user interface to simplify inputs.

Power Tower (Direct Steam)
--------------------------

Replaced estimated receiver thermal efficiencies in UI with estimated receiver heat loss. The heat loss is typically
less dependent on flux, so specifying this value allows the thermal efficiency to automatically vary with flux.

Solar field modeling changes detailed for the Molten Salt Power Tower.

Automatic optimization of the receiver during simulation runs is still under development and is not yet available. However,
optimization within the interface on the Solar Field page is available.

Molten Salt Technologies (Physical Trough, Power Tower, Linear Fresnel)
-----------------------------------------------------------------------

The field-to-storage HX sizing for the controller previously
specified that the HX duty should be (solar multiple = 1 ) \*
(power block design thermal power). This makes the size adequate if
the field always deploys to the power block first, then
only balance is sent to storage. This is not desired behavior,
especially for peaker type plants. The HX sizing is now simply
the solar field design power (solar multiple) \* (power block design
thermal power). Additionally, the off-design heat exchanger
model has been updated to maintain physical performance when the
load is near the design load.

.. _geothermal-1:

Geothermal
----------

Removed resource temperature and depth lookup via NREL web service in accordance with a recommendation from NREL geothermal
technology experts.

.. _solar-water-heating-4:

Solar Water Heating
-------------------

Updated algorithm to use a triple-mode solar water tank model that more accurately models actual system behavior.

Added support for subhourly simulations (1, 5, 10, 15, 20, 30 minute, and 1 hour)

Added electric load input option for economic analysis.

.. _residential-and-commercial-financial-models-5:

Residential and Commercial Financial Models
-------------------------------------------

Added a new building load estimator for the residential and
commercial financial models that generates an hourly electric
load profile based on information you provide about the building
size and electric appliances. Requires monthly electric load
data to scale normalized profile to actual energy usage.

Removed the Commercial PPA and Independent Power Producer financial models because they were redundant with each other, and
with the more representative Single Owner model, which is now the only option for modeling PPA projects with a single
owner.

Improvements to the utility rate database (URDB) inputs for the residential and commercial financial models and calculations
to work with Version 3 of the OpenEI database.

Utility-scale Models
--------------------

New financial metrics for the Power Purchase Agreement financial models: SAM now reports both the levelized cost of energy
metric based only on project costs after taxes and incentives but without the developer's return on investment, and the
levelized PPA price, which accounts for both costs and the developer's return

Major update to the Send to Excel with Equations feature.

.. _scripting-1:

Scripting
---------

Scripts are no longer embedded in project files.

Scripting language is the new LK language which is much faster and more powerful.

New functions in standard library to create and export plots of data, reading/writing CSV files, HTML dialogs, web access
and geocoding, new graphical user interface commands, Excel automation (Windows only), and direct access to the underlying
simulation engine (SSC).

Added a script-based Macro system to support integrated extensions to SAM.

SAM 2014.1.14: January 2014 (Update)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a maintenance update to SAM 2013.9.20 that addresses several bugs and other issues It does not add any new
features.

Photovoltaic
------------

Fixed a bug for the photovoltaic models with southern hemisphere weather files with the Tilt=Latitude option
enabled that caused SAM to generate a simulation error message.

Fixed a bug for the PVWatts system model with 1-axis tracking. The bug caused the model to enable backtracking with a GCR of
-1. This caused unrealistic results for a few hours of the year, but did not noticeably affect the system's total annual
output.

Corrected the Flat Plate PV model's inverter library entry for the Toshiba (TMEIC) 500U. The correct values for the
voltage and current ratings are: Vdcmax = 600 V, Idcmax = 1600 A, MPPT low = 320 V, and MPPT high = 550 V.

Updated the CEC inverter library to the January 2014 version.

Fixed a mistake in the PDF report for photovoltaic models with Commercial PPA, Utility IPP, or Single Owner financing in
Specify PPA Price mode to show meaningless symbols for the PPA price value. This issue does not affect values that
appear in the SAM user interface -- it only affects the PDF report.

Changed the Flat Plate PV default assumption on the Array page
for ground reflectance to not use the albedo value from the
weather file by default because the albedo values in the standard
weather files tend to be unreliable. By default, SAM uses the
0.2 value specified in the monthly albedo input table instead of
reading hourly values from the weather file.

Concentrating Solar Power
-------------------------

Fixed a bug in the energy loss diagram for concentrating solar
power models (CSP) with a large portion of the thermal energy
to the power block supplied by the fossil backup system that caused
the energy loss diagram to display an incorrect value for
the System Output to Grid quantity.

For the linear Fresnel model, fixed an issue with the the collector azimuth angle variable on the Solar Field page being
disabled with the collector incidence angle table option on the Collector and Receiver page.

Fixed an issue that caused the power tower model with thermocline storage to report an incorrect warning about the storage
fluid temperature being outside of its operating range.

For the physical trough model, fixed some problems that caused simulations to fail for low temperature applications. The
evacuated tube convergence algorithm could fail to converge when the DNI is zero and the HTF is losing energy to its
surroundings, and some of the property lookup routines failed when field temperatures fell below zero degrees Celsius.

For the physical trough and generic solar system concentrating
solar power models Thermal Storage page, fixed the units in
the thermal storage capacity label. The correct units are MWht, not
MWt. This is a mistake in the user interface label and does
not affect simulations.

Fixed a mistake in the Linear Fresnel PDF report that showed aperture area of a single collector instead of the total
aperture area.

Updated the TRNSYS source code files.

.. _residential-and-commercial-financial-models-6:

Residential and Commercial Financial Models
-------------------------------------------

Fixed a bug for the residential and commercial financial models, that caused SAM to apply the percent of annual
output factor from the Performance Adjustment page twice.

For the residential and commercial financial models with net metering, corrected the Sales/Purchases without System value
for January. (The values for February through December were correct.) This caused the Net Savings with System value in the
Metrics table to be incorrect.

For the residential and commercial financial model results, corrected the monthly Energy Rate Charge in [mmm]
($) label, and added the missing Year 1 monthly sales/purchases without system ($) variable is missing. The
label should be Energy Charge in [mmm] ($).

For the residential and commercial financial models with the
Normalize supplied load profile to monthly utility bill
data option on the Electric Load page enabled, SAM used the
original, non-normalized peak load to calculate monthly
demand charges. With this update, SAM uses the normalized peak load
values to calculate the demand charges. Although the old
behavior is appropriate with 10- or 15-minute load data when the
magnitude of the normalized data is similar to the original
data and you want to preserve the original peak values, we decided
to change it because it may be unexpected behavior, and is
not appropriate when you normalize the load to much higher or lower
magnitudes than the original data.

Fixed the formula in the cash flow spreadsheets with equations for residential and commercial financial models to correctly
calculate state ITC amount. The formula in Cell B51 was incorrect.

SAM 2013.9.20: September 2013
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version includes several improvements to photovoltaic
modeling, utility rate and net metering calculations, bug fixes
to concentrating solar power models, a revised and much faster solar
water heating model, new wake modeling for wind farms, and
updated default input values. In addition, this version comes with
several new sample files to demonstrate various SAM
features, including how to calculate net metering impacts with
different utility rates and electric loads.

.. _general-8:

General
-------

Fixed an issue on OS X systems when editing the library and weather file search path.

.. _residential-and-commercial-financial-models-7:

Residential and Commercial Financial Models
-------------------------------------------

Updated utility rate inputs to be compatible with Version 2 of the OpenEI Utility Rate Database. For each utility in the
database, SAM groups rate structures by category to make it easier to find a specific structure.

Net metering algorithm now carries over monthly credits and applies an optional year end sell rate. (Older versions of SAM
assumed used hourly accounting for net metering.)

Renamed hourly and monthly results variables, and removed duplicate and confusing variables.

.. _photovoltaic-1:

Photovoltaic
------------

Added two options to model inverters not in the Sandia inverter database: The datasheet option allows you to
enter parameters from a manufacturer datasheet. The part load curve option allows you to enter part-load efficiency
curve that is linearly interpolated across the range of operation.

For self-shading shading of 1-axis trackers, a new ground coverage ratio input replaces the row width and spacing inputs to
avoid being able to model systems with inconsistent array layout inputs.

Added estimation of output reduction due to self-shading of 1 axis trackers using an approximation that calculates geometric
beam irradiance shading fraction from the GCR and assumes a long row. The diffuse sky and reflected irradiance is also
reduced based on the view factor reduction to adjacent rows. Note: does not currently model nonlinear impact of electrical
mismatch losses

Added additional detail in energy loss diagram to better characterize the source of each loss: irradiance reductions due to
shading, inverter operation and clipping, and others.

Improved labeling of results variables.

Bug fix to backtracking algorithm: The rotation angle limit is now applied correctly for backtracked systems.

Bug fix in fixed tilt system self-shading code: The diffuse irradiance view factor reduction calculations were modified
slightly to more accurately characterize the light blocking on the array.

Added an option in PVWatts to change the mounting configuration of the PV array. The selection of an open rack or
roof-standoff mounting system adjusts the installed nominal operating cell temperature (INOCT) of the modules in the
system.

.. _solar-water-heating-5:

Solar Water Heating
-------------------

Replaced the solar water heating performance model with a faster model.

.. _concentrating-solar-power-1:

Concentrating Solar Power
-------------------------

Physical trough, power tower, direct steam tower and linear
Fresnel: Fixed an error in the powerblock startup calculations.
This is not a problem for simulations where the startup time is less
than the simulation timestep, so the default cases
(direct steam power tower and linear Fresnel) that use this model
are not affected. Models where the startup time is greater
than the simulation time may have been underpredicting the required
startup energy.

Physical trough, power tower, direct steam tower and linear
Fresnel: Corrected an error in the hybrid cooling model that
decreases annual results using hybrid cooling model between by as
much as 0.5% for the Molten Salt Tower and 1.75% for the
Physical Trough model. These are worst case differences at small wet
cooling fractions. Larger wet cooling fractions result in
smaller differences.

Dish Stirling: Fixed a significant bug that underestimated
shading losses in the default dish Stirling model. The annual
energy in the default case decreases by 6.5%. As the spacing between
modules increases, the difference between the old and new
codes decreases and eventually disappears.

Empirical trough and the generic solar system model: the turbine startup energy calculation was not properly deducting
energy for systems without thermal energy storage and with a large turbine thermal startup requirement. For these cases, the
error in annual output could be greater than 12% (output went up after the fix), depending on the power block startup energy
requirement. The impact for the default values in both the GSS and Empirical trough was minimal.

Generic solar model: the optical efficiency table is accompanied
by a checkbox indicating whether the table should be
interpolated or should simply return the nearest value. The checkbox
values were backwards i.e. checking interpolate
actually caused the model to return the nearest value, and vice
versa. This bug would be most significant in cases with
significant optical efficiency variation over small changes in the
angular inputs, or in tables with a small number of values
that differ substantially from each other. For the default case,
change in annual output was less than 1%.

The powerblock model for the physical trough and molten salt
power tower models was not correctly accounting for powerblock
startup times greater than 1 hour (note the default is 0.5). The fix
will cause annual energy output to decrease for these
cases. For example, in the molten salt power tower, using the
defaults and changing the powerblock startup time to 1.5 hours
decreases the annual energy by about 4%. This bug will also affect
custom simulations using timesteps less than the powerblock
startup time.

Improved TRNSYS convergence of molten salt power tower during periods when the solar field is off but storage and the
powerblock are still operating. The default case shows around a 0.2% increase in annual energy, and an elimination of TRNSYS
convergence errors reported in the log file.

Improved startup and pumping power calculations in direct steam power tower. Default case annual energy increases around
0.8%.

Fixed bug in air cooling model that caused fan parasitics to switch to 0 in some cases where it was operating. The fixed
version reduces the default power tower models annual energy output around 1%.

.. _wind-8:

Wind
----

Created the ability to import a user defined turbine layout for a wind farm.

Added the ability to model wind wake effects using the Eddy-Viscosity model.

Added the ability to model wind wake effects using the Park model.

Worked with Mistaya Inc to develop the capability for Windographer software to export data in SAM's .srw wind resource file
format.

Increased the maximum number of turbines that can be analyzed in a wind farm from 250 to 300.

Added a capability to display information about siting considerations for a particular location. SAM communicates with the
NREL Wind Prospector online tool to obtain the information.

.. _geothermal-2:

Geothermal
----------

Fixed a bug where certain calculation were not updated during parametric runs.

Fixed a bug that caused SAM to calculate the temperature drop for an EGS resource incorrectly.

SAM 2013.1.15: January 2013 (Update)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM 2013.1.15 is a maintenance update that addresses a few issues with the previous version. For a full description of new
features in this version, see the SAM 2012.11.30 description below.

.. _general-9:

General
-------

Fixed an issue that caused SAM to stop responding when viewing SRW wind data files with invalid format.

Updated Performance Adjustment variable group names so they appear correctly in lists of variables (parametrics, etc.).

Resized boxes on input pages to fit notes in orange font.

Fixed an issue with the advanced utility IPP financial models that prevented the model from converging when the LCOE was
very small.

Fixed an issue with the cash flow Send to Excel feature that that caused percentages to be incorrectly displayed in Excel,
and caused inaccurate escalation rate calculations.

Disable Excel Exchange by default.

Fixed a problem with TOU adjustment rates issues with net metering and load.

Made the utility rate database window resizable and added scrollbars to make it easier to see long names and
descriptions.

Fixed a problem with cash flow graphs in reports for the advanced utility IPP financial models that caused multiple bars to
appear on the graph.

Fixed a problem with the weather data reader so that it correctly exports data headings.

Fixed a problem with the Save with Hourly Options result that prevented SAM for saving data with unusual numbers of values
(e.g., 200 values for statistical simulation).

SAM now prompts you for a folder to save reports, and uses the .zsam file location by default instead of automatically
saving it in the user folder without any feedback.

SamUL
-----

PtOptimize() script function returns true/false depending on whether solar field optimization was successful.

Added PtGetOutput() script function to return all messages from running PTGen via PtOptimize()

.. _photovoltaic-2:

Photovoltaic
------------

Flat plate PV did not read shading inputs for Subarray 1.

Flat plate PV azimuth angle value did not import correctly from files saved with older.

Updated report templates for PV systems to correct formatting and logic issues.

Flat plate model now correctly imports azimuth angle value from files saved with SAM 2011.12.2.

Fixed HCPV system costs page icon in navigation menu.

Fixed tab order on photovoltaic input pages.

Removed parentheses in photovoltaic model results labels so they appear correctly in the time series data viewer.

.. _concentrating-solar-power-2:

Concentrating Solar Power
-------------------------

Update TRNSYS source code files for all CSP models

Fixed a problem with the empirical trough model when the solar field stow angle was 180 degrees.

Fixed bug that results in Empirical trough model annual output error of 4%.

Improved input variable labels on Solar Field page for exact area and actual aperture.

Fixed issue with empirical trough model solar field initial temperature. This had a very small impact on the system's
total annual output.

.. _wind-9:

Wind
----

Fixed a problem for the wind cost model that caused values to be zero.

Corrected the wind PTC default value.

Added a limitation of 300 turbines to the total number of turbines in a system with an error message when more turbines are
entered into the farm layout.

.. _geothermal-3:

Geothermal
----------

Fixed a problem with energy values shown in the Geothermal report.

.. _biopower-1:

Biopower
--------

Fixed a problem with the energy loss diagram that caused labels to overlap.

SAM 2012.11.30: November 2012
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version includes several improvements to the existing
performance models and enhancements to the user interface. The
new solar wizard makes it easier to get started using SAM to model
solar projects. This version also includes a new energy loss
diagram, new report templates, and an option to export cash flow
data to Excel with formulas.

.. _general-10:

General
-------

Automatically download tax credit and incentive data for United States locations from the online Database of State
Incentives for Renewable Energy (DSIRE).

SAM Solar Wizard steps you through the minimum number of inputs to set up a basic analysis for a photovoltaic (PVWatts),
parabolic trough (Empirical Trough), or solar water heating system.

Loss diagrams for all performance models except wind and geothermal show where energy losses occur in the system.

Export cash flow tables to Excel with formulas for the residential, commercial, commercial PPA, and utility IPP financial
models.

Expanded list of retail electricity rate structures available for download from the NREL OpenEI utility rate database.

Improved Performance Adjustment page (replaces Annual Performance page) to model curtailment, degradation, availability, and
other operating losses with new hour-by-month factors.

New report templates.

Reorganize navigation menu and input page to make input variables easier to find.

Remove graph sliders and optimization analysis options because the Parametric Analysis option is a better and more
transparent way to show variation of results over a range of inputs.

Change the default value of some input variables to better match current market conditions, including changing the default
analysis period from 30 years to 25 years.

PVWatts System Model
--------------------

Bug fix: When Force tilt=latitude is enabled, surface tilt is now calculated as the absolute value of the location
latitude to ensure that for locations in the southern hemisphere, the tilt value is positive. (SAM requires all array tilt
values to be positive.)

Bug fix: PVWatts will generates an error message if it reaches an invalid data line in the weather file. Previously, PVWatts
may have continued through the file using unpredictable values as input.

Flat Plate PV Component Models
------------------------------

Enhancement: SAM has been upgraded to allow specifying up to four subarrays with different orientation, shading, and soiling
inputs. The subarrays are assumed to have the same number of modules per string, and are wired in parallel connected to the
single inverter.

Enhancement: Relative air mass is reported as an hourly output.

Enhancement: System performance ratio (factor) is calculated in a consistent manner between flat plat and CPV systems. See
documentation for more details. The calculation method does not change the results.

Enhancement: DC and AC derate factors are specified as fractions, not percentages. This makes the derate convention
consistent with PVWatts. SAM automatically updates values in files created with older versions with percentage values.

Bug fix: The single point efficiency (SPE) inverter occasionally produced nameplate power with zero incident irradiance. An
example is shown in the plot below, for a 4 kW SPE inverter.

In the default residential PV system with a 4 kW SPE inverter, this fix reduces the annual AC kWh by approximately 1.8
%.

Bug fix: The Sandia PV module model did not correctly use the
database-specified temperature coefficients. The temperature
coefficients used were A = -3.56, B=-0.075, dT=3 when the Use
database values option was selected in the user interface. For
the default utility scale system in SAM, the fix increases the
annual output using the default Sandia module by approximately
0.3 %.

Bug fix: The Sandia PV module model now reports the module temperature as the ambient temperature under zero irradiance
conditions. The previous version of SAM reported 0 deg C under some zero irradiance conditions. This does not change the
results.

Bug fix: Night-time inverter parasitic power is now correctly deducted from the total energy. This reduces SAM's annual AC
power production by approximately 0.03 %, using the utility scale default system as a reference.

Updated module and inverter databases.

High-X Concentrating PV System Model
------------------------------------

Revision: Tracker power is now specified as a fraction of nameplate capacity. As a result, tracker parasitic power by
default scales with system size. Project files from old versions of SAM are upgraded accordingly.

Updated inverter database.

Concentrating Solar Power Models (CSP)
--------------------------------------

Updated HTF fluid library.

Bug fix: The molten salt power tower now allows the user to locate a heliostat on the x axis.

The runner and header heat loss calculations in the physical trough model were modified. The reported losses should
approximately double for the default case. The net electric energy output will decrease around .1 - .2% for most cases. For
systems with a larger number of field subsections, the effect will be slightly more significant.

Therminol 66 and 59 are now available as heat transfer fluids for the trough models. Rough guidelines for temperature
limits of all HTFs are provided in the user interface.

The absolute pipe roughness in the external molten salt and direct steam tower models was changed from 1.5E-5 to 4.5E-5 m.
This change results in a high pumping parasitic, but in regards to net energy is largely offset by an increased thermal
efficiency caused by improved convective heat transfer coefficients.

The flux map DELSOL returns for the external molten salt and direct steam receiver now reports normalized values that
indicate for each node 1-12 the fraction of the total power incident on the receiver that is absorbed. This revised metric
corrects for a mismatch between the DELSOL reported field efficiency and the efficiency that can be calculated from the flux
map output.

.. _wind-10:

Wind
----

A new Turbine input page with an option to either choose a turbine power curve from a library of commercially available
turbines, or to specify a turbine's performance characteristics (coefficient of power, tip-speed ratio, etc.)

Location Lookup now accesses the NREL Eastern Wind Integration Dataset in addition to the Western dataset.

Added a set of representative wind data files for typical resource and terrain types in the United States.

Improved the wind data file format to make it easier to edit with spreadsheet software and changed the wind data file
extension from .swrf to .srw.

The Wind System Costs page has a new option to estimate costs for onshore and offshore wind projects using the NREL Capital
Cost model.

New hour-by-month performance adjustment inputs to facilitate modeling operating losses due to curtailment and maintenance
downtime.

.. _biopower-2:

Biopower
--------

The Feedstock page now provides access to the U.S. Billion Ton Update study for dedicated energy crops in addition to the
agricultural residue database.

A new life-cycle emissions model estimates greenhouse gas emissions of biomass collection, transport, pre-processing,
combustion, and CO2 re-uptake.

Geothermal Power
----------------

Added the ability to estimate plant costs for binary geothermal units.

SAM 2012.5.11: May 2012
~~~~~~~~~~~~~~~~~~~~~~~

For SAM 2012.5.11, we added one new performance model, made
several improvements to algorithms to decrease simulation run
times, and made the usual bug fixes, usability improvements, and
documentation revisions. We re-wrote the flat plate PV
simulation engine to reduce computational overhead and remove the
dependence on the TRNSYS engine. The new code runs nearly 10
times faster than previous versions of SAM. This will reduce the
time required for parametric and other analyses that require
multiple simulations. The new High-X Concentrating Photovoltaic
(HCPV) model is a new performance model that replaces the CPV
option on the Module page. The new model includes CPV-specific
derate factors, an estimate of spectral effects, and is
structured to allow us to improve the model as new data and
algorithms for CPV systems becomes available. We have created a
set of new report templates for PV, wind, biopower, and geothermal
systems that show a summary of key inputs and results in a
PDF document.

Photovoltaic Models
-------------------

Improved simulation engine for faster simulations. (see Summary above for details)

Remove CPV model option from Module page, and replaced it with a new High-X Concentrating PV (HCPV) performance model. The
new HCPV model includes CPV-specific derates, and air mass correction to simulate spectral effects.

Rename Component-based PV model to Flat Plate PV model.

Change azimuth angle convention for Flat Plate PV model to be consistent with PVWatts convention: 0=N, 90=E, 180=S, 270=W.
If you use SAM 2012.5.11 to open a file you last saved in a previous version, SAM correctly converts the azimuth value for
northern hemisphere locations only. For southern hemisphere locations, you should change the azimuth value yourself.

Improve sun position calculations for Flat Plate PV model. This causes a small difference in the Flat Plate PV system output
due to more accurate sun position calculation (ref comparison of TRNSYS to PVWatts sunpos calculation - PDF report from
U.Wisc)

Improved Flat Plate PV Backtracking algorithm. Simplified inputs significantly, improved runtime, results show better
comparison with other tools (PVsyst)

For the Flat Plate PV, CEC Performance Model with User Entered Specifications module model option, allow temperature
coefficients to be entered either in A/degC, V/degC, or %/degC.

.. _general-11:

General
-------

Improved usability of P50/P90 simulation option. Report P50/P90 values for all results shown in the Metricsd table on the
Results page.

Developed new report templates for PV, biopower, wind, and geothermal systems.

Improved web update system to add automatic notification when software updates are available.

Added Library() function in SamUL to query library types and entry names. One application of the function is to
run through different modules in one of the PV module libraries.

Biomass Power
-------------

Some Input pages rearranged and streamlined for clarity.

Changed Emissions Comparison page to Life-Cycle Impacts page.
The new page allows users to see the emission effects of
transportation fuel, transportation mode, and pre-processing method.
The overall greenhouse gas emissions results are displayed
graphically, broken into general categories such as transportation,
collection, combustion, and biomass uptake. This page does
not affect the performance or cost model in any way. It is purely
informational.

Removed the option to specify biomass power plant capacity. This option was confusing to users, since intuitively, this
option would back-calculate the amount of biomass used to achieve a plant of the specified size and the corresponding
collection radius. The back-calculation of feedstock radius is not a current capability of SAM Biopower.

.. _concentrating-solar-power-csp-models-1:

Concentrating Solar Power (CSP) Models
--------------------------------------

Fixed stow angle bug in empirical trough model. Should not affect results unless stow angles were set to an angle much less
than 180 .

Thermal storage tank heater efficiency was not being applied to tank heater parasitic losses. This should have minimal
impact on results for most cases because tank heater is typically not frequently used and default heater efficiency is
0.99.

Fixed thermal storage tank freeze protection calculations. Impact on annual performance results should be negligible.

Optical efficiency did not include cosine effects. Modified version will significantly change optical efficiency results.
Annual energy is not affected.

Fix field defocus calculation for physical trough and molten salt power tower models. Impact on gross and net annual energy
is negligible. May have larger impact (~5%) on the energy from the field in some cases.

Fix bug in empirical trough model that allowed storage to over-discharge under some conditions. Impact on annual energy
should be negligible.

Fix bug in freeze protection calculations for the physical trough model. Impact should be negligible for systems with low
freeze protection loads. Systems with higher freeze protection loads may see a noticeable decrease in required freeze
protection energy.

.. _wind-11:

Wind
----

Consolidate small-scale and large-scale wind models into a single Wind Power model that uses the swrf wind data format and
files from the Western Wind Dataset.

SAM 2011.12.2: December 2011 (Update)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM 2011.12.2 is a maintenance update that addresses a few issues with the previous version including:

.. _general-12:

General
-------

Error when opening zsam files by double-clicking the file name

Issue with power tower models in Mac version.

Handling of PV soiling derating factors when opening files created in older versions.

For a full description of new features in this version, see the SAM 2011.11.29 description below.

SAM 2011.11.29: November 2011
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM 2011.11.29 adds three new technologies and several tools and capabilities, in addition to bug fixes, usability
improvements, and documentation revisions (see below for detailed descriptions):

New technologies
----------------

CSP Linear Fresnel

CSP Direct Steam Power Tower

Biomass Power

New capabilities
----------------

Model PV modules with spec sheet data

Monthly soiling derates for PV systems

P50/P90 analysis

Model a generic system using hourly or sub-hourly generation profile as input

New tools
---------

Integrated time series data viewer

Report Generator

Case Compare

Photovoltaic Component-based
----------------------------

Ground Reflectance (albedo): Removed ground reflectance with snow
input (albedo w/ snow) from the Array page. If the weather
file contains valid albedo values between 0 and 1, they are in the
simulation. Otherwise, the single ground reflectance value
from the Array page is used. When comparing results from the current
version with older versions of SAM, for locations that
experience snowfall, a weather file format that includes snow depth,
(Boulder CO.tm2 for example), and the default ground
reflectance inputs, the annual output predicted by the current
version will be typically slightly lower than the output
predicted by older versions. Older versions of SAM predicted higher
system output during snow cover periods by assuming that
all PV panels would be cleared of snow, and that the more reflective
ground would reflect more diffuse radiation onto the
panels. The current version does not make this assumption for the PV
component based models, but rather limits the input to a
single average ground reflectance input.

Tilted Surface Irradiation Models: Removed the Hay and Davies, and Perez 1988 diffuse irradiation models options from the
Array page. This change simplifies the user interface while keeping
the Isotropic, HDKR, and Perez 1990 models available.If you
open a SAM file created with an older version of SAM and using the
old Hay and Davies option, SAM will change the setting to
the HDKR option in the new version . Similarly, the Perez 1988
option from the old version upgrades to the Perez 1990 option in
the new version. The HDKR model typically will predict slightly
higher diffuse irradiation on a tilted surface than the Hay
and Davies model because of its treatment of additional diffuse
irradiation components. The Perez 1990 model is based on
coefficients extracted from a larger input dataset than the Perez
1988 model.

Irradiance Component Input Option: Removed the option on the
Array page for using total (global) horizontal and diffuse
components of irradiation. The current options are total and beam,
and beam and diffuse. By default, SAM uses beam and diffuse
from the weather file. There should be no difference in results
between the current version and older versions for weather file
with self-consistent irradiance values. This change simplifies the
user interface while still providing sufficient options for
nearly all modeling needs.

Soiling: Added an option for entering monthly soiling derates.
The derate is applied equally to all components of the
calculated plane-of-array (POA) irradiance before the DC module
power is calculated. This is different from previous versions
of SAM, in which the soiling derate was applied to the DC output of
the module. The new approach is more consistent with real
systems, where soiling blocks irradiance before reaching the module,
which both reduces POA irradiance and affects cell
operating temperature. The approach in the new version tends to
slightly reduce the system's annual output compared to the
approach in older versions, assuming a constant soiling derate for
all months.

Shading: Added an option to import Solar Pathfinder Month x Hour or Obstruction table shading input files.

PV Module Model: Added a CEC Performance Model with User Entered Specifications option on the Module page. This model
allows the user to enter module datasheet specifications directly into SAM, which calculates coefficients to drive the CEC
performance model. The coefficients are calculated using a method detailed in the SAM help system.

PV Databases: The CEC PV Module, Sandia Module, and Sandia Inverter databases were updated to the latest available
versions.

Monthly Soiling Derates: The PV Component-based model includes an option to specify 12 monthly values for the soiling derate
factor on the Array page.

Backtracking: The backtracking option in SAM was temporarily removed for this version because the SAM team did not have
confidence that it was working correctly in all configurations. We are developing an updated algorithm and expect it to be
faster and easier to use for future versions of SAM.

.. _photovoltaic-pvwatts-1:

Photovoltaic PVWatts
--------------------

Cell Temperature: PVWatts in SAM now reports cell temperature as equal to ambient temperature (dry bulb) when the sun is
down. Previous versions reported 999 during non-solar hours. This change has no impact on results.

Handling of Snow Depth: PVWatts in SAM now uses the snow depth
data in a TMY2 weather file (if available) to adjust the
ground reflectance (albedo) from 0.2 (no snow) to 0.6 (with snow
cover). This change makes the results match the online PVWatts
V.1 exactly for TMY2 files. When comparing results with previous
versions of SAM, for locations with measured snow cover, this
change will typically increase the annual output from a system
because snow cover hours will reflect more diffuse irradiance
from the ground surface onto the panels. This assumes that panels
are always cleared of snow.

Physical Trough System
----------------------

Improved the iterative solution algorithm for solar field mass
flow rate. The previous release used the
successive-substitution method for determining convergence of the
HTF mass flow rate in the solar field. While generally
successful, this method is prone to error in situations where the
initial guess values do not closely resemble the final
converged solution. This release uses the hybrid false-position
iterative method which has proved to be significantly more
robust and stable than the previous method in solving for the solar
field mass flow rate. The modified solver algorithm also
allowed us to place thermal inertia and heat loss calculations
inside of the primary iteration loop, thus improving the
accuracy of the solar field calculations and reducing solver time.
Potential impacts include: This is the most significant
modification to the solar field algorithm in this release. The more
accurate solver ensures energy/mass/temperature balance in
a wider range of operating conditions, and thus impacts the annual
and hourly energy production. The net impact was shown to
slightly reduce annual energy output and have a varying effect on
individual time step calculations.

Adjusted the design-point mass flow rate in the solar field to
more correctly calculate the absorbed thermal energy and
thermal losses from the loop at design. This change slightly
increases the thermal performance of the solar field according to
the design-point calculations, thus setting the design mass flow
rate to a higher value. Potential impacts include: Defocusing
of the solar field caused by mass flow exceeding its maximum limit
will occur less frequently. The design-point mass flow rate
is also used to size the piping and header diameters. This change
leads to potentially slightly larger diameter piping,
increasing the thermal inertia of the modeled plant, increasing the
piping thermal losses, and decreasing the pumping parasitic
through the headers.

Updated pipe diameter sizing algorithm to correctly handle systems where the number of SCAs in the loop is 1 or 2 (8 is the
default value). Potential impacts include: Addresses a bug causing the simulation to crash in this scenario.

Correction to header piping length calculation. Potential impacts include: The piping length was calculated to be
artificially low. This fix correctly calculates the header length, thus impacting the thermal inertia of the solar field and
piping thermal heat losses. The net effect of this changed was observed to decrease the annual energy production for the
default system.

Added an algorithm to the solar field model that avoids recalculating the solar field during multiple iterations if the
inputs to the module do not change. Potential impacts include: Improved simulation speed, no changes to performance
calculations are anticipated.

Correction to the calculation of optical efficiency during the
first and last hour of the day in which the solar field
operates. Potential impacts include: This fix applies only to
simulations with sub-hourly time steps. During this type of
simulation, thermal energy produced during the first and last time
step of solar field operation is reduced slightly, depending
on the time step duration.

Optical end loss (light reflected at an angle off the end of
the last collector in a row that is not absorbed) was
previously applied as an average optical loss to each collector in
the loop. The code was updated to apply this loss to only
the relevant collectors in the loop. Potential impacts include:
Total solar field optical efficiency is unchanged, though the
energy absorbed by each collector will be distributed slightly
differently. This slightly impacts thermal losses from the
receivers, but is likely unnoticeable in annual simulation results.

Improved the solar field freeze protection algorithm. The
previous release assumed that freeze protection would be applied
locally in the calculations for the solar field to maintain the HTF
at the desired freeze-protection temperature. In other
words, the freeze protection was (unrealistically) injected into
any given calculation node in the solar field that fell
below the minimum temperature. The model was updated to a predictive
algorithm that applies fossil backup to heat the HTF at
the inlet of the solar field to a sufficient temperature such that
the HTF exiting the loop matches the desired freeze
protection temperature. This more accurately captures thermal losses
in the circulating fluid and the required energy
contribution from the auxiliary fossil boiler. Potential impacts
include: Improved accuracy in freeze protection calculations,
including thermal losses and fossil energy requirement.

Updated pressure loss calculation in the hot and cold headers to scale more accurately with mass flow rate. Potential
impacts include: Reduced pumping parasitic requirement at part-load operation.

Improved evacuated tube receiver thermal loss model to account for the temperature gradient across the SCA instead of
calculating the thermal losses at an average temperature only. Potential impacts include: Improved accuracy in thermal loss
calculations for the solar field receivers.

Removed the minimum power cycle restart requirement. The functionality of this value was compromised by the turbine startup
time requirement and the turbine startup energy requirement, and thus is no longer necessary. Potential impacts include:
None

Corrected a bug in the thermal storage system that allowed a negative mass in the tank under some conditions. This caused
the simulation to crash. Potential impacts include: No performance impacts. Reduced likelihood of simulation crash in some
situations.

Corrected the calculation for thermal load into the power block
during standby mode operation. The previous release
underestimated the thermal requirement to maintain standby
operation. Potential impacts include: Increased thermal requirement
during standby operation (if applicable). Impact is expected to be
minimal for most cases.

Improved steam property lookup accuracy. Potential impacts include: Improved convergence of steam property calls and
calculation of steam-related values such as heat rejection evaporative loss and power cycle water blowdown loss.

Power Tower System
------------------

Implemented a bug fix for calculating heliostat efficiency at
negative solar azimuth angles. The previous release used the
heliostat field component from the TRNSYS STEC library to
interpolate solar field efficiency from a matrix defined by solar
azimuth and zenith angles which artificially limited the solar
azimuth angle to zero. The TRNSYS definition for solar azimuth
angles applies a range of -180 deg to +180 deg (North to North,
clockwise), thus the solar field efficiency was incorrectly
interpolated at an azimuth angle of 0 deg for all solar azimuth angles
less than zero (morning hours). Potential impacts include:
Solar field optical efficiency was over-estimated for most systems
during morning hours, resulting in an annual over-estimate
of approximately 3% for the default external receiver system and 5%
for the default cavity-north system. This is the most
significant modification in the tower models for this release.

Added outputs to report HTF pressure drop across the receiver. Potential impacts include: The user is now able to review the
HTF pressure drop, including gravity and frictional head loss.

Removed the minimum power cycle restart requirement. The functionality of this value was compromised by the turbine startup
time requirement and the turbine startup energy requirement, and thus is no longer necessary. Potential impacts include:
None

Corrected a bug in the thermal storage system that allowed a negative mass in the tank under some conditions. This caused
the simulation to crash. Potential impacts include: No performance impacts. Reduced likelihood of simulation crash in some
situations.

Corrected the calculation for thermal load into the power block
during standby mode operation. The previous release
underestimated the thermal requirement to maintain standby
operation. Potential impacts include: Increased thermal requirement
during standby operation (if applicable). Impact is expected to be
minimal for most cases.

Improved steam property lookup accuracy. Potential impacts include: Improved convergence of steam property calls and
calculation of steam-related values such as heat rejection evaporative loss and power cycle water blowdown loss.

Generic Solar System
--------------------

Updated the optical table interpolation algorithm to allow simultaneous interpolation of multiple tables. This change was
made to support the Linear Fresnel system, which also uses the algorithm developed within the GSS model. Potential impacts
include: No performance impacts are expected.

Improved performance calculations to determine the design-point
solar field optical efficiency based on the actual solar
zenith at solar noon on the summer solstice. This allows the model
to choose a more representative design-point mass flow rate.
Potential impacts include: Minimal changes to the annual energy
production.

.. _generic-system-1:

Generic System
--------------

SAM's Generic System model now allows you to provide either hourly generation data or sub-hourly data with up to
one-minute resolution as input, which makes it possible to use results from an external performance model with SAM's
financial models.

.. _solar-water-heating-6:

Solar Water Heating
-------------------

Collector Specification: Updated simulator to correctly use number of collectors specified by user. Also made collector
database on user interface searchable.

Draw Profile: Added option to scale hourly draw profile to an average daily hot water draw of kg/day.

.. _geothermal-power-1:

Geothermal Power
----------------

Removed some unnecessary inputs, and updated default values.

Allow pump work as input.

Added the power block component, which to allow for modeling of a geothermal system using SAM's power block
component.

.. _biomass-power-1:

Biomass Power
-------------

The utility-scale biopower model was developed by the SAM team with internal funding from NREL. The model accesses online
NREL databases of biomass resource to model a biopower plant.

Tools
-----

Integrated time series data viewer. SAM's new Time Series Data
Viewer replaces DView for displaying graphs of time
series data. The new viewer is integrated into the user interface,
and runs on both Windows and Mac OS versions of SAM. DView
is still available in SAM via the Results menu.

Report Generator. The new report generator allows you to design
and generate reports in PDF format with tables of both input
data and results, along with text, and images. Once you design a
report template, you can use it with different SAM files. This
should facilitate generating reports for project reports.

Case Compare. The Case Comparison window shows inputs and results from all of the cases in a file in a single, editable
table. You can quickly identify differences between cases, and update values of inputs directly from the window.

Search Box: For inputs that use a list to populate values (weather file, PV modules and inverters), a search box makes it
possible to type a few characters of the input name.

Cost and Financing
------------------

Land Cost for Solar Systems: The land cost category on the System Costs page for PV and CSP systems is now linked to the
land area input on the Array or Solar Field page.

Weather Files
-------------

Location lookup allows for choosing specific year or typical DNI year file. Previous versions only downloaded
the typical year file.

SAM 2011.6.30: June 2011 (Update)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version includes updated default input values across all technologies and several bug fixes.

Financing
---------

Reorganize and rename financing options in the Technology and Market window: The Utility Independent Power Producer (IPP)
option is equivalent to the Utility IPP options in pre-2011.5.4 versions of SAM, and the new utility financing options are
listed under the Advanced Utility IPP Options heading.

Removed the mid quarter MACRS depreciation option from the Utility IPP and Commercial PPA options.

Added a custom depreciation option for the new utility financing options to facilitate analysis of depreciation schedules
other than those typically available under U.S. tax law.

Corrected a problem with the units in the Production Tax Credit calculation that affected analyses involving the PTC. SAM
2011.5.23 and earlier incorrectly overestimated the PTC amount by a factor of 1000.

Fixed convergence issues in the PPA price calculation algorithm in the Utility IPP and Commercial PPA models when used with
Wind, Generic, and PVWatts system models.

PV
--

Updated CEC PV Module and Updated Sandia PV Inverter databases.

Corrected a problem with reading Solmetric SunEye shading files.

For the advanced Multiple Systems simulation option, corrected the PV capacity factor and system performance factor
calculation.

.. _general-13:

General
-------

Updated default input values across all technologies.

Added PDF export for single graphs.

Corrected a problem with SAM window placement on a multiple monitor setup.

Corrected a problem with axis labels in graphs of parametric results that included variable linkages. SAM now displays the
value of one of the linked variables instead of using long labels that include all variable names and values.

Added a search box to selection windows such as the Choose Parametrics window. You can now type a few characters of a
variable name to find it in the list.

Fixed a problem with the Mac version on some older 32-bit Mac OS systems.

.. _weather-data-2:

Weather Data
------------

Improved error checking for weather files, and in the TMY3 weather file creator

Power Tower
-----------

Fixes issue in power tower model and north field calculations.

Corrected units for DNI in hourly results.

.. _wind-12:

Wind
----

Added the No Financials option for the Utility Scale Wind technology option.

SAM 2011.5.23: May 2011 (Update)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version addresses a problem with the PV operation and maintenance default values and includes some formatting changes to the Help
system.

SAM 2011.5.4: May 2011
~~~~~~~~~~~~~~~~~~~~~~

This version adds many new features in addition to fixing bugs in
existing models. It adds several new utility scale
financing models that more accurately represent some common
financial structures used for renewable energy projects than SAM's
original Utility-IPP model. Other improvements include a generic
system model that can accept hourly energy input from other
models, enhanced temperature correction algorithms for the CEC PV
module model for various mounting options, more detailed
accounting for PV cost details, a new model for calculating
geothermal co-production in conjunction with an oil or gas well,
and a utility scale wind model that can directly access a large
database of hourly wind data at various hub heights.

General New Features
--------------------

Improved welcome screen with news feed to show updates about upcoming versions, bug fixes, webinars, and other SAM-related
information

Additional graphs automatically generated to show cost breakdowns in stacked cost per watt ($/W) and LCOE (cents/kWh) on y
axis

New sample file for showing scripting (SamUL) capabilities

Updated reference manual for scripting (SamUL)

.. _financial-models-1:

Financial Models
----------------

Added an All Equity Partnership Flip financing structure that
represents a power generation project with a tax investor
and developer and no debt. You specify a target IRR and year or
target PPA price with the allocation of cash and tax benefits
to each partner before and after the target year. SAM calculates the
project PPA price and the NPV and IRR for each
partner.

Added a Leveraged Partnership Flip financing structure that
represents a power generation project with a tax investor and
developer and debt. You specify a target IRR and year or target PPA
price with the allocation of cash and tax benefits to each
partner before and after the target year. SAM calculates the project
PPA price and the NPV and IRR for each partner.

Added a Sale Leaseback financing structure that represents a
power generation project with two partners. The tax investor
purchases the project from the developer and leases it back to the
developer. The tax investor receives lease payments from the
developer along with tax benefits and incentives. The developer
keeps any excess cash flow after operating costs and lease
payments are made. You specify a target IRR and year or target PPA
price and lease terms. SAM calculates the project PPA price
and the NPV and IRR for each partner.

Added a Single Owner financing structure that represents a
power generation project with one owner. The owner receives all
of the cash and tax benefits from the project. The owner may be the
original project developer or a third party investor if the
developer is unable to use the tax benefits or lacks sufficient
capital to construct the project. You specify debt terms, and a
target IRR and year or target PPA price. SAM calculates the project
PPA price, IRR and NPV.

Merged Residential Cash and Residential Loan/Mortgage financing options into single Residential model. To model cash
financing, set the debt fraction to zero.

Merged Commercial Cash and Commercial Loan financing options into a single Commercial model. To model cash financing,
set the debt fraction to zero.

Renamed Third Party Ownership to Commercial PPA to better reflect calculations. User can now specify either the IRR
target or the First Year PPA price and solve for the other.

Merged Utility IPP , Utility Dispatch , and Utility Bid Price options into single Independent Power Producer option
that handles all three cases. The user can specify the IRR target or the First Year PPA price, along with time-dependent
valuation factors (dispatch).

Simplified inputs for tax credits and incentives to remove unnecessary complexity.

Handling of property tax fixed. Previous versions overemphasized the valuation of property taxes, and an improved method is
implemented.

.. _weather-data-3:

Weather Data
------------

Bug fix for copy/paste into TMY3 creator wizard on Mac OS X

.. _photovoltaics-3:

Photovoltaics
-------------

Updated CEC and Sandia module databases to most recent available versions.

Updated Sandia Inverter database to most recent available version.

Added detailed thermal modeling of mounting options. Available only with the CEC module model.

Added an array backtracking algorithm for one axis tracking systems.

Fixed calculation of nominal DC array output for the CEC module model. Does not affect energy calculations.

CPV tilt and azimuth inputs now work correctly with the selected tracking mode

Updated CPV module page with temperature corrections to Pmp, added cell temperature calculation

Updated curve fit calculations for Sandia inverter input page

Updated default values for a, b, dT mounting coefficients on Sandia module page

Fixed tilt=latitude bug for southern hemisphere

.. _solar-water-heating-7:

Solar Water Heating
-------------------

Added a database of SRCC solar thermal collectors.

Added input for maximum auxiliary power to allow for modeling of larger systems

Added input for circulation pump power

.. _parabolic-trough-physical-2:

Parabolic Trough (Physical)
---------------------------

The solar field inlet HTF temperature calculation was corrected
to better model plant behavior during shutdown. Previously,
the field inlet temperature remained tied to the power block outlet
temperature when the solar field was not producing power,
but thermal storage was providing energy for power cycle operation.
This prevented accurate modeling of nighttime solar field
cool down behavior. Potential impacts include: Observed field
inlet/outlet temperature during nighttime operation for systems
with thermal storage or auxiliary fossil backup, required solar
field startup energy

The pipe sizing algorithm was modified to match calculated piping
diameters to a common piping schedule. The piping schedule
is now selected based on the minimum available schedule diameter
that exceeds the calculated diameter requirement. Potential
impacts include: Piping thermal loss, piping pressure drop and
parasitic power requirement, solar field thermal inertia (from
modified piping HTF volume calculation)

The field piping thermal inertia term on the Power Cycle page was split into three separate inertia terms and moved to the
Solar Field page. The new inputs include specific thermal inertia terms for the cold header piping, hot header piping, and
collector loop piping. Potential impacts include: Solar field transient behavior during startup and shutdown

The convergence tolerance on the solar field and controller
algorithms was tightened. The former tolerance values proved to
be insufficient for simulations with sub-hourly time steps.
Potential impacts include: Annual energy output, longer simulation
time

An error in the collector loop HTF temperature calculation was corrected. The error related to the fraction of energy
involved in warming/cooling the HTF during non-steady-state operation. Potential impacts include: Transient behavior during
startup, shutdown, and rapidly changing DNI levels

The mass flow rate calculation within the solar field was
improved. Previously, it was possible for the mass flow rate to
indicate convergence with small remaining convergence error in the
field energy balance. The improved calculation eliminates
this error. Potential impacts include: Small differences in the
solar field mass flow rate or thermal energy output

On the Thermal Storage page, an input was added for the Hot tank
heater set point. This allows the tank model to maintain a
temperature set point for the auxiliary heater for both the cold and
hot storage tanks separately. Potential impacts include:
Differences in predicted tank heater parasitics, difference in
thermal energy storage performance throughout the year

On the Power Cycle page, an input was added for the Minimum power block restart time. This input allows the user to control
the amount of time that the power cycle will require to resume producing electricity if the cycle trips during daytime
operation because of low solar resource. Potential impacts include: Annual electricity production, electricity production on
partially cloudy days

On the Power Cycle page, an input was added for the Turbine inlet
pressure control method. This allows the user to select
the power cycle performance model as either fixed pressure or
floating pressure. Previously, only fixed pressure operation was
modeled. Potential impacts include: None for fixed pressure mode;
For floating pressure mode - power cycle performance during
part load operation, power cycle outlet temperature during thermal
storage discharge

On the Thermal Storage page, an input was added to allow the user
to select the auxiliary fossil backup dispatch mode.
Options now include Minimum backup level, and Supplemental
operation. Previously, only the Minimum backup level mode was
included. The Supplemental operation mode allows fossil backup to
provide thermal energy to the system in addition to thermal
energy provided by the solar field or TES. The maximum rate of
energy delivery is the fraction of design point power specified
in the Fossil Fill Fraction inputs under the Thermal Storage
Dispatch Control group, and the total fossil contribution plus the
energy from the field and TES cannot exceed the corresponding Turbine
Output Fraction value. Potential impacts include: None for
Minimum backup level. For supplemental operation modified fossil
backup control

The solar field defocusing algorithm was modified to more
accurately model field defocusing during over-design operation.
Modifications improved the model's ability to avoid excessive
defocusing and reduce dumped energy during over-design operation.
These changes are most prominent in systems where frequent
defocusing is required. Potential impacts include: Annual
electricity output, solar field performance during over-design
operation, dumped thermal energy, model convergence issues

The measure for power cycle over-design operation within the
model was changed from total thermal energy input to HTF mass
flow rate. This change reduces instances of excessive HTF mass flow
rate through the power cycle and more accurately simulates
real plant operation strategies. Potential impacts include: Observed
power block HTF mass flow rate, peak electric power
generation

The solar field inlet HTF temperature calculation was corrected
to better model plant behavior during shutdown. Previously,
the field inlet temperature remained tied to the power block outlet
temperature when the solar field was not producing power,
but thermal storage was providing energy for power cycle operation.
This prevented accurate modeling of nighttime solar field
cool down behavior. Potential impacts include: Observed field
inlet/outlet temperature during nighttime operation for systems
with thermal storage or auxiliary fossil backup, required solar
field startup energy

The field configuration option on the Solar Field page was
modified to allow the user to specify the number of subfields
rather than the configurations I or H . This change improves the
model's ability to capture the piping performance of a
wider range of field layouts. Along with this change, internal
piping layout algorithms were updated to accommodate the range
of subfield selections. Potential impacts include: Piping thermal
loss, piping pressure drop and parasitic power requirement,
solar field thermal inertia (from modified piping HTF volume
calculation)

.. _parabolic-trough-empirical-2:

Parabolic Trough (Empirical)
----------------------------

The startup criteria in the plant control algorithm incorrectly
restricted startup to conditions above the power cycle
dispatch requirement, when startup should have been allowed above
the minimum power block operation level. This problem was
corrected to represent a more realistic startup procedure. Potential
impacts include: Small impact on annual energy production,
small impact on electricity output during morning operation

.. _power-tower-1:

Power Tower
-----------

The existing control strategy encompassing all plant operation
and dispatch decisions was deemed to be limited in
flexibility and prone to convergence errors. To improve the tower
model and ensure a common simulation platform for all CSP
models, the previous control algorithm was replaced by the algorithm
developed for the Physical Trough model. This switch has
significant benefits in ensuring the quality of the results over a
wide input variable space, and provides improved results
especially in simulation cases where solar multiple, thermal storage
sizing, or auxiliary fossil backup sizing deviate
significantly from the optimal. This modification applies to both
the external and cavity receiver models. Potential
impacts include: Improved solar field defocusing control, improved
plant startup behavior, improved thermal storage or
auxiliary fossil backup dispatch control, increased annual
electricity output especially for systems with no storage

Added fossil dispatch control scheme option on the Thermal Storage page. Refer to the Physical Trough release notes

The pumping power requirement for HTF passing through the power
block was previously calculated on a thermal power basis
meaning the total thermal power passing through the pump was used to
calculate an estimated electrical pumping power
requirement. This calculation was replaced by an estimate on a mass
flow basis, where total pumping power is given in terms of
kJ per kg of HTF (equivalently kW per kg/s of mass flow). This
change makes the tower model consistent with the pumping power
convention for the parabolic trough model. Potential impacts
include: Modified parasitic pumping power for HTF through the
TES/Power Cycle system

Added sliding pressure operation for the power cycle. Refer to the Physical Trough release notes

An error in the natural convection thermal loss calculation used the wrong temperature value for the conductivity of air. A
second error in the calculation used the wrong active area for convection between the tube wall and the HTF. Both were
corrected and resulted in a minor difference in annual energy production. Potential impacts include: Small difference in
thermal losses from the external receiver.

Wind Systems
------------

Addition of utility scale wind model. The SAM team has added a
utility scale wind option for SAM users. The new option uses
the same algorithm to determine wind farm output (including a simple
wake analysis), but provides a new list of turbines, new
financing options, and a new source for wind resource data. The new
wind resource data is more granular in coverage and
provides for measurements at hub heights more appropriate for
utility scale wind turbines.

Added several hourly wind resource data files. More can be accessed via the online database connection that is integrated
into SAM.

Enhancements to the small scale wind turbine model. The small
scale model has been improved to run more quickly and some
minor bugs have been addressed. In addition, new turbines have been
added to the Small Scale Wind turbine library, bringing the
total to 16.

.. _geothermal-4:

Geothermal
----------

Addition of geothermal co-produced electricity model. The
Co-Production model in SAM estimates power output from
co-production resources based on the resource temperature and flow
rate and the power plant model chosen. The power plant model
calculates the plant net power output based on either the thermal
efficiency or utilization efficiency assumed for the power
plant.

Generic Model
-------------

Can accept hourly energy production values as input, calculated by an external model.

SAM 2010.10.8: October 2010
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This version adds many new features in addition to making
improvements to existing ones. It adds a non-solar technology
models for small wind and geothermal power systems. It also adds
options for modeling complex utility rate structures for
residential commercial projects, and time-of-use pricing through the
use of energy payment allocation factors for utility
projects. This version allows modeling of two new utility financing
options. For PV systems, this version adds a more
sophisticated shading model. The Results page has been improved to
make it easier to display and export hourly data, regardless
of operating system (Windows or Mac), and to display hourly data for
simulations involving multiple runs such as parametric
analyses.

Major Changes
-------------

New generic solar system model for concentrating solar power systems that models the solar field using a table of optical
efficiency values that you specify.

New geothermal power system model.

New small scale wind power model.

Addition of time of dispatch and bid price utility financing models to allow modeling of projects in California under the
California Public Utilities Commission rules, and to calculate the project internal rate of return when you know the power
purchase price.

For PV systems, addition of a more sophisticated array shading model that allows you to import shading data from the PVsyst
simulation software and SunEye shading analysis tool.

New utility rate page for complex rate structures for residential and commercial projects that includes time-of-use rates,
peak demand charges, and tiered rate structures.

Improved electric load modeling for residential and commercial projects.

New cavity receiver model for power tower systems.

Remove electric storage option from photovoltaic model pending improvements.

.. _general-14:

General
-------

New hourly data browser on Results page allows viewing and exporting of hourly data tables on both Windows and Mac computers
(does not require Microsoft Excel) and viewing of hourly results for analyses that involve multiple model runs such as
parametric analyses.

Improvements to statistical simulation option to add integrated regression and selection of multiple outputs.

Better error checking with display of warnings for some simulation issues on Results page (warning message button at top
right of Results page.)

Residential and Commercial Projects
-----------------------------------

An improved utility rate model allows specification of a range of rate structures from simple flat rate with net metering,
to complex structures with time-of-use rates, demand charges, and tiered rates. The model also allows rate structures to be
imported from NREL's new rate database hosted on the OpenEI website.

Financing page includes an input to specify salvage value.

Utility Projects
----------------

New models for utility financing options allowing specification of power purchase price as an input, and use of energy
payment allocation factors to model time-of-use pricing.

New inputs on the Financing page for specifying construction interest costs that accrue before the analysis period
begins.

Financing page includes an input to specify salvage value.

Photovoltaic Systems
--------------------

Array sizing calculator allows you to specify the system DC capacity, and automatically calculates the values for modules
per string, strings in parallel, and number of inverters on the Array page.

Improved array shading model allows specification of beam shading factors using a table of 8,760 hourly values, a 24 by 12
month by hour table, or an azimuth by altitude table. Other options are to import shading data from the PVsyst simulation
software or the SunEye shading analysis too. Finally the new shading model allows modeling of self-shading within the array
(without backtracking).

Detailed derate factors can be specified directly on the Array page.

Improved error checking to help ensure inverter and array capacities are matched.

Updated module and inverter databases.

The electric storage (battery) model has been removed in this version.

Array page allows specification of the land area as a multiple of the total module area for reference. (Land area is not
accounted for in cost calculations.)

New option for using updated Perez model for calculating incident radiation from data in weather file.

Fixed a problem with the way SAM calculated inverter AC output for hours when the DC input exceeded the inverter
capacity.

Removed reference variables for CEC module model.

Power Tower Systems
-------------------

Improvement of condenser modeling in power block.

New cavity receiver model.

Add hybrid cooling.

Renaming of input and output variables for clarity.

Improved modeling of parasitic losses.

Add storage bypass valve control.

Receiver maximum flow rate based on HTF properties and maximum over design operation fraction.

New solar multiple input variable.

New receiver control variables.

Added specification of the land area as a multiple of the total module area for reference. (Land area is not accounted for
in cost calculations.)

Parabolic Trough Systems
------------------------

Improvement of condenser modeling in power block. (physical model)

Added specification of the land area as a multiple of the total module area for reference. (Land area is not accounted for
in cost calculations.)

Improved modeling of fossil fuel usage for systems with fossil backup.

Add hybrid cooling. (physical model)

Improved heat loss calculation. (empirical model)

.. _generic-solar-system-1:

Generic Solar System
--------------------

Added new model to facilitate modeling of concentrating solar power systems not handled by parabolic trough, power tower,
and dish-Stirling models. Allows modeling of solar field based on a table of efficiency values.

Geothermal Power Systems
------------------------

New model for utility-scale geothermal power generation projects based on the U.S. DOE's GETEM model.

Small Scale Wind Systems
------------------------

New model for small wind systems that consist of one or more turbines for residential and commercial projects. Current
version relies on wind speed data in solar TMY data.

SAM 2010.4.12: April 2010 (Update)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This update corrects several issues with SAM 2010.3.31.:
--------------------------------------------------------

System summary: Units for the total installed cost per capacity were incorrect for photovoltaic, solar water heating, and
generic systems.

Financing: Projects with commercial or utility financing and an analysis period less than the depreciation period caused
simulations to fail. (This was also true for previous versions.)

Incentives: Production-based incentives (PBI), production tax credits (PTC), and investment tax credits (ITC) were
incorrectly calculated.

| Photovoltaic systems: The annual energy production was incorrect for systems with bipolar inverters from the Sandia inverter
  database.
| Solar water heating: System energy savings were incorrectly calculated.

Parabolic trough (physical): Receiver heat loss calculation was incorrect for receivers with lost vacuum or hydrogen
leakage.

Power tower: Water usage was incorrectly reported, values of the solar field delivered energy and power block input energy
were incorrectly reported in the hourly results worksheet, and default values of some input variables were incorrect.

Parabolic trough and power tower: Power block capacity-based costs were incorrectly calculated based on the net capacity
instead of the gross capacity.

The update includes the following improvements:
-----------------------------------------------

Several new functions and improvements to the SamUL scripting language, including the ability to run scripts from the
command line.

Revised user documentation.

SAM 2010.3.31: March 2010
~~~~~~~~~~~~~~~~~~~~~~~~~

This version adds a solar water heating model and a new parabolic
trough model to Solar Advisor, improves the display of
graphs on the results page, and adds capabilities to the statistical
analysis simulation option. It also includes interface
improvements to the photovoltaic module and inverter pages, improves
modeling of temperature effects on module performance, and
includes the latest module and inverter databases from the CEC and
Sandia. Finally, this version adds a weather data download
feature for U.S. locations, and a function to create weather files
in TMY3 format.

.. _general-new-features-1:

General New Features
--------------------

New solar water heating model for residential systems.

Tax credits and incentives can be defined as annual schedules.

Add levelized cost of energy (LCOE) with and without incentives.

New functions in SamUL

Graph thumbnails on Results page.

Input window for loads (photovoltaic, solar water heating) adds options for specifying load data: Daily profiles by month,
cut and paste from clipboard.

.. _weather-data-4:

Weather Data
------------

Location lookup option automatically downloads NREL Solar Prospector weather data for U.S. locations using an address or
latitude and longitude.

TMY3 creator facilitates creating custom weather files.

.. _pv-1:

PV
--

Simple load and storage models added to all photovoltaic models. Was only available in PVWatts model in previous
versions.

Revised default rating conditions for concentrating photovoltaic (CPV) model. This will affect capacity-based calculations
including LCOE when the module cost is specified in $/W.

Temperature correction added to concentrating photovoltaic (CPV) model, and improved in simple efficiency and Sandia models
to include more options for specifying mounting options.

Improved layout of Module page for CEC and Sandia model. Display I-V curve and other changes.

Improved layout of Inverter page. Display inverter efficiency curves and rename input variables.

Azimuth tracking option added to photovoltaic models.

New hourly output data columns added to hourly outputs.

Corrected system performance factor calculation for systems with shading.

Parabolic Trough
----------------

New physical model for parabolic trough systems. Original trough model renamed empirical trough model. Both
trough models are available in the current version.

.. _power-tower-2:

Power Tower
-----------

New field land area variables on Heliostat Field page.

Variables on input pages reorganized.

Generic Fossil
--------------

Allow generic fossil input variables to be parametric variables.

Version 2009.10.2: October 2009
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _general-new-features-2:

General New Features
--------------------

Significantly faster model runs and smaller project file sizes.

Both Windows and Mac OS X-Intel versions are available.

A SCIF file importer for opening files saved with previous versions of SAM.

A new graphs page can display up to four graphs simultaneously and has new controls for creating and modifying graphs.

Tornado-chart type analysis is built-in as a specific Sensitivity analysis.

Optimization allows you to maximize or minimize a metric with respect to inputs.

| Multiple sub-systems allows you to model systems made up of two or more subsystems.
| Weather model reads TMY3, EPW, and TMY2 files.

Scripting and batch-mode capabilities with Excel and other programming languages.

.. _pv-2:

PV
--

Update CEC module database (10/2)

Simple Efficiency Model allows for multiple radiation level/efficiency pairs (Flat Plate and CPV)

Incorporated PVWatts model

Dish-Stirling
-------------

No updates.

.. _parabolic-trough-1:

Parabolic Trough
----------------

Update HCE library.

Improvements to thermal storage dispatch strategies.

Can accept custom HTF fluids using a table-lookup mechanism.

.. _power-tower-3:

Power Tower
-----------

Improved heliostat layout mechanism and optimization wizard.

Can accept custom HTF fluids using a table-lookup mechanism.

Version 3.0: June 2009
~~~~~~~~~~~~~~~~~~~~~~

New Features
------------

Power tower model.

.. _pv-3:

PV
--

Simple array shading model.

Add energy flow graph to standard graphs on Results page.

Update Sandia inverter database.

Update CEC module database.

.. _dish-stirling-1:

Dish-Stirling
-------------

No updates.

.. _parabolic-trough-2:

Parabolic Trough
----------------

Update HCE library.

Tracking of backup boiler fuel cost in cash flow and cost of energy calculations.

Improve calculation of backup boiler fuel usage.

.. _general-15:

General
-------

Updated help system and user guide, including topics for power tower model.

Minor changes
-------------

Update and rename sample files.

Improvements to user-defined variable implementation.

For cases using Excel optimization (utility financing model), Excel file runs in background by default.

Improve handing of SCIF files.

Show incentive tax details by default on Incentives page.

General improvements to speed up run times.

Version 2.5: November 2008
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _new-features-1:

New Features
------------

Parabolic dish-Stirling engine model.

Time-of-use utility rates.

User documentation available as both context-sensitive Help and PDF file.

Automated optimization of power purchase agreement (PPA) price escalation rate and load fraction for projects with IPP and
Utility financing.

.. _pv-4:

PV
--

Revise inverter and array sizes in sample file.

Update Sandia inverter database.

Update CEC module database.

Change module and inverter database format to Excel from MDB.

Display voltage and capacity variables on array page to facilitate inverter and array sizing.

.. _dish-stirling-2:

Dish-Stirling
-------------

Add dish-Stirling sample file.

Implement dish-Stirling model.

.. _parabolic-trough-3:

Parabolic Trough
----------------

Remove unused variables from user interface: number of receivers per SCA, HTF flow control, HTF night flow control, and
turbine start-up time.

Rename time-of-use variables to time-of-dispatch and move them from Utility page to Storage
page.

.. _general-16:

General
-------

Remove Utility - IOU option from list of financing types on Financials page, and rename Utility - IPP option to
IPP and Utility .

Improve graphing options.

Change utility rate units from cents/kWh to $/kWh.

Add Excel-based optimization of PPA escalation rate and debt fraction for IPP and Utility financing.

Improve installation on Windows Vista.

Improve sliders interface.

.. _minor-changes-1:

Minor changes
-------------

Fix time format issue with parabolic trough time of dispatch schedule.

Improve file compression to minimize file size.

Allow long variable names in parametric analyses.

Revise WACC calculation.

Read all significant digits from linked spreadsheets.

Version 2.0: June 2008
~~~~~~~~~~~~~~~~~~~~~~

.. _pv-5:

PV
--

Implement new inverter performance model: Sandia Performance Model for Grid-connected PV Inverters

Removed previous curve-fit inverter model

Implement new photovoltaic module performance model: California Energy Commission Performance Model.

Update Sandia PV Array Performance Model with additional modules. (Was King Model in previous versions.)

CSP
---

Implement dry cooling capability for CSP systems.

Add new Solel UVAC3 HCE to CSP model library.

.. _general-17:

General
-------

Add generic technology option: The simple heat-rate model allows comparisons between solar technologies and conventional
fuel-based technologies in all markets.

Add Third Party Ownership option to commercial projects.

Allow operation and maintenance costs and annual degradation rates to be entered on a year-by-year basis.

Add new results workbook that stores complete set of calculated metrics, hourly data, monthly averages, and annual averages
to facilitate reviewing results in Excel.

Improve display of results page: Move results button to bottom of navigation menu; replace single button with three, Results
Summary, Spreadsheet, and Time Series Graph; implement new Run Analysis button to replace results pending status.
Also add new Results menu.

Replace LCOE stacked bar graph with stacked cost graph to correct error in LCOE cost breakdown under certain conditions.

Add several new standard graphs, including monthly output, monthly inverter efficiency (PV only), and energy flow (CSP
only).

Add new graphs that are available when one or more Independent parametric groups are defined.

Create new compressed file format (SCIF) that stores only inputs in small files for easier file sharing. Cases can be
imported and exported from SAM files in the SCIF format.

.. _minor-changes-2:

Minor changes
-------------

Minor bug fixes

Reduce required disk storage space by deleting workbooks and other files from temporary folder when closing SAM.

Add transformer derate category to Array page for PV systems.

Display weighted average cost of capital (WACC) on Financials page.

Improve internal rate of return (IRR) calculation for utility and third party ownership projects.

New Fixed (per year) operation and maintenance category.

New folder (Samples\\Financial_Spreadsheets) contains sample workbooks illustrating Solar Advisor financial calculations.
Workbooks are also posted on the Solar Advisor website.

Solar Advisor opens with an empty window instead of automatically opening previous file.

Add close file button to menu bar.

Version 1.3: October 16, 2007
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _general-18:

General
-------

Add capability to use EnergyPlus Weather file format (EPW) files for weather data.

Add capability to add weather files to the collection of built-in files.

Add weather data viewing tools, links to weather data web sites, and Help button to the Climate page.

Add ground reflectance with snow input variable to Array page for PV systems. SAM Applies the snow ground reflectance value
during hours that the weather data indicates there is snow on the ground.

Fix a bug in the inverter model for low part-load operation.

Add waterfall graph capability in DView on Results page for CSP systems.

Update user guide.

Improve overall performance of model.

Other changes
-------------

Add start up mode settings (File, Settings) and change tab names in Settings window.

Add help button to Climate page that opens a help page describing weather file options.

Improve search algorithm that finds a solution for systems using utility financing.

Improve calculation of number of TRNSYS runs displayed in information message.

Change hourly output buttons on Results page for CSP systems.

Improve handling of files created by different versions of SAM.

Improve automatic graph scaling.

Improve message for users attempting to run SAM when the Windows language setting is not English.

Improve automatic scale on sliders.

Improve file navigation for File, Open and File, Save As commands.

Version 1.1: August 10, 2007
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _general-19:

General
-------

Photovoltaics Array: Add detailed derate factors.

Results: Add Slider column to results summary table that displays output measures based on the position of visible
sliders.

Results
-------

Change the units of ITC and IBI incentives that appear with sliders and in graphs from % Max to %.

Improve the functioning of the Notes box.

For utility systems with IOU financing, remove first year PPA from the results summary table.

For utility IPP financing, improve the LCOE calculation algorithm.

Incentives
----------

Cursor changes to an hourglass while resetting default market values.

Correct default settings for tax details to show that for residential systems, all utility incentives are taxable, and that
utility incentives do not reduce the ITC basis.

Photovoltaics Array
-------------------

Changed default total derate factor in sample files to 84% to match PVWATTS.

Costs
-----

Correct the way the sales tax rate and module cost per unit values are displayed.

Parametrics
-----------

Fixed a bug related to incentives.

Fixed a bug related user-defined variables.

External Spreadsheets
---------------------

Improved handling of missing workbooks.

Fixed a bug related to working with external spreadsheets.

Menus
-----

Add the Release notes command to Help menu to display a list of version numbers and the new features and fixes associated
with each version number.

Reorganize the list of commands on the Case menu.
