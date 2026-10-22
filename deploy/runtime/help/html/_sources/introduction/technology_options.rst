Performance Models
==================

SAM's performance models run simulations to calculate the power system's electrical output. The :doc:`financial model <../introduction/fin_overview>` uses the system's output to calculate the project annual cash flows and financial metrics.

Photovoltaic Systems
~~~~~~~~~~~~~~~~~~~~

SAM models grid-connected photovoltaic systems that consist of a photovoltaic array and inverter. The array can be made up of flat-plate or concentrating photovoltaic (CPV) modules with one-axis, two-axis, or no tracking.

SAM offers three models for photovoltaic systems (see :doc:`Choose Models <../getting-started/choose_models>` for instructions):

Detailed Photovoltaic
---------------------

The :doc:`detailed photovoltaic model <../detailed-photovoltaic-model/pv_overview>` calculates a grid-connected photovoltaic system's electrical output using separate module and inverter models. It requires module and inverter specifications along with information about the number of modules and inverters in the system. You can either provide your own module and inverter specifications from a manufacturer's data sheet, or choose a module and inverter from libraries. The detailed photovoltaic model models the effect of temperature on module performance, and has options for calculating shading and other losses in the system. The model also includes a system sizing calculator to help you determine the number of modules and inverters in the system.

Use the detailed photovoltaic model when you have information about the equipment that will be used in the system.

PVWatts Model
-------------

The :doc:`PVWatts model <../pvwatts/pvwatts>` is an implementation of NLR's popular `online photovoltaic calculator <https://pvwatts.nlr.gov/>`__. It models a grid-connected photovoltaic system using a few basic inputs to describe the system's nameplate capacity, array orientation and mounting type, and system losses. PVWatts makes internal assumptions about module and inverter characteristics for three types of modules. SAM's implementation of PVWatts includes options for modeling shading that are not available with the online version.

Use the PVWatts model for preliminary project analysis before you have information about the type of equipment you plan to use in the system, or for other analyses that require a reasonable estimate of a photovoltaic system's electrical output.

High Concentration PV
---------------------

The :doc:`high concentration photovoltaic <../high-concentration-photovoltaic/hcpv_overview>` model is appropriate for grid-connected photovoltaic systems with high concentration photovoltaic (HCPV) modules. The concentrating photovoltaic model uses separate models to represent the module and inverter. It requires information about the design of the concentrator and efficiency of the cell at different irradiance levels. For the inverter model, you can either use specifications from a manufacturer data sheet, or choose an inverter from a library.

The following table summarizes the three photovoltaic models:

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - 
     - Detailed Photovoltaic
     - PVWatts
     - Concentrating Photovoltaic
   * - Flat plate crystalline and thin film modules
     - •
     - •
     - 
   * - Low concentration photovoltaic (LCPV) modules
     - •
     - •
     - 
   * - High concentration photovoltaic (HCPV) modules
     - 
     - 
     - •
   * - Array output (DC)
     - • 
     - • 
     - • 
   * - Inverter output (AC)
     - • 
     - • 
     - • 
   * - System sizing assistant
     - •
     - 
     - 
   * - DC/AC sizing ratio
     - • 
     - • 
     - 
   * - Specify system nameplate capacity
     - • *
     - • 
     - 
   * - Specify numbers of modules and inverters
     - • *
     - 
     - •
   * - Specify separate module and inverter parameters
     - • 
     - 
     - • 
   * - Multiple subarrays with optional voltage mismatch calculator
     - • 
     - 
     - 
   * - Tracking options (fixed, one- or two-axis tracking)
     - • 
     - • 
     - 
   * - Adjustable tracker rotation limit for one-axis tracking
     - •
     - 
     - 
   * - Azimuth axis tracking
     - •
     - 
     - 
   * - Shading factors for nearby objects
     - • 
     - • 
     - • 
   * - 3D shade calculator
     - • 
     - • 
     - 
   * - Self-shading for one-axis trackers
     - • *
     - • 
     - 
   * - Backtracking for one-axis trackers
     - • *
     - • *
     - 
   * - Self-shading for fixed arrays (no tracking)
     - • *
     - 
     - 
   * - Temperature effects on module performance
     - • 
     - • 
     - • 
   * - DC and AC loss inputs
     - • 
     - • 
     - • 
   * - System availability
     - • 
     - • 
     - 
   * - Loss diagram
     - •
     - 
     - 
   * - Estimate land cost from array area
     - • 
     - 
     - •

\ :sup:`*`\ Optional feature

Battery Storage
~~~~~~~~~~~~~~~

SAM's electric :doc:`battery storage <../battery-storage/battery_storage>` is available with the Detailed Photovoltaic and Custom Generation Profile models for either front-of-meter (FOM) or behind-the-meter (BTM) applications. The electric battery storage model is also available for standalone batteries. A simplified version of the battery model is available with the PVWatts model.

Detailed PV-Battery
-------------------

The detailed PV-Battery model couples a grid-connected photovoltaic system to a battery bank, which can be connected to either the DC side of the photovoltaic inverter, or the AC side of the system. The model must be used with a financial model, and is modeled as a behind-the-meter application for the Residential, Commercial, and Third Party Ownership financial models, and a front-of-the-meter application for any of the Power Purchase Agreement (PPA) or Merchant Plant financial models. For behind-the-meter applications, there is an automatic sizing and dispatch option that uses NLR's REopt API.

PVWatts-Battery
---------------

The PVWatts-Battery model is a simplified implementation of the PV-Battery model for behind-the-meter applications that is available with the Residential, Commercial, or Third Party Ownership financial models and includes the automatic sizing and dispatch option that uses NLR's REopt API.

Custom Generation Profile-Battery
---------------------------------

The Custom Generation Profile-Battery model couples a grid-connected power system to a battery bank. The power system is represented by a generation profile that can be from a different model or measured from a real system. The model must be used with a financial model, and is modeled as a behind-the-meter application for the Residential, Commercial, and Third Party Ownership financial models, and a front-of-the-meter application for any of the Power Purchase Agreement (PPA) or Merchant Plant financial models.

Standalone Battery
------------------

The standalone battery model is for a battery bank connected to the grid that charges and discharges from the grid.

Hybrid Systems
~~~~~~~~~~~~~~

SAM's hybrid system models combine generation profiles of two or more power generation systems with battery storage.

PVWatts Wind Battery
--------------------

Combines the PVWatts, Wind Power, and Standalone Battery models to represent a hybrid system with photovoltaic and wind subsystems. The combined output of the subsystems can deliver power to the grid, battery, or load, depending on the system design.

PVWatts Wind Fuel Cell Battery
------------------------------

Combines the PVWatts, Wind Power, Fuel Cell, and Standalone Battery models to represent a hybrid system with photovoltaic, wind, and fuel cell subsystems. The combined output of the subsystems can deliver power to the grid, battery, or load, depending on the system design.

Photovoltaic Wind Battery
-------------------------

Combines the Detailed Photovoltaic, Wind Power, and Standalone Battery models to represent a hybrid system with photovoltaic and wind subsystems. The combined output of the subsystems can deliver power to the grid, battery, or load, depending on the system design.

Custom Generation PVWatts Wind Fuel Cell Battery
------------------------------------------------

Combines the Custom Generation Profile, PVWatts, Wind Power, Fuel Cell, and Standalone Battery models to represent a hybrid system with custom generation profile, photovoltaic, fuel cell and wind subsystems. The custom generation profile can represent any type of power generation equipment. The combined output of the subsystems can deliver power to the grid, battery, or load, depending on the system design.

Thermal Storage
~~~~~~~~~~~~~~~

Electric TES
------------

The :doc:`electric thermal energy system <../electric-thermal-energy-storage/etes>` model is for a thermal energy system (TES) that uses grid power as a power source to generate heat.

Pumped TES
----------

The :doc:`pumped thermal energy system <../pumped-thermal-energy-storage/ptes>` model is for a thermal energy system (TES) that uses grid power as a power source to generate heat.

Concentrating Solar Power
~~~~~~~~~~~~~~~~~~~~~~~~~

The concentrating solar power (CSP) models are for grid-connected thermal power plants that use solar energy to generate steam to drive an electric power generation plant.

Note. The Integrated Solar Combined Cycle (ISCC) model was removed in SAM 2020.2.29. The model is still available as part of the Legacy version SAM 2018.11.11.

Parabolic Trough (Physical Model)
---------------------------------

The :doc:`physical trough model <../csp-physical-trough-model/troughphysical_overview>` calculates the electricity delivered to the grid by a parabolic trough solar field that delivers thermal energy to a power block for electricity generation, with an optional thermal energy storage system. The physical trough model characterizes many of the system components from first principles of heat transfer and thermodynamics, rather than from empirical measurements as in the empirical trough system model. While the physical model is more flexible than the empirical model (see below), it adds more uncertainty to performance predictions than the empirical model.

Parabolic Trough (Empirical Model)
----------------------------------

The :doc:`empirical trough model <../csp-empirical-trough-model/troughempirical_overview>` models the same type of parabolic trough system as the physical trough model, but uses a set of curve-fit equations derived from regression analysis of data measured from the SEGS projects in the southwestern United States, so you are limited to modeling systems composed of components for which there is measured data. The model is based on Excelergy, originally developed for internal use at at the National Laboratory of the Rockies.

Molten Salt Power Tower
-----------------------

A :doc:`molten salt power tower <../csp-power-tower-molten-salt/mspt_overview>` system (also called central receiver system) consists of a heliostat field, tower and receiver, power block, and optional storage system. The field of flat, sun-tracking mirrors called heliostats focus direct normal solar radiation onto a receiver at the top of the tower, where a molten salt is heated and pumped to the power block. The power block generates steam that drives a conventional steam turbine and generator to convert the thermal energy to electricity. 

Direct Steam Power Tower
------------------------

The Direct Steam Power Tower model is not included in versions released after November 2020. The model is available in the legacy SAM 2020.2.29 version, which you can download from the SAM `website download page <https://sam.nlr.gov/download>`__.

Linear Fresnel Molten Salt
--------------------------

A :doc:`molten salt linear Fresnel <../csp-linear-fresnel-molten-salt/mslf_overview>` system consists of a field of slightly curved or flat Fresnel reflectors that focus light on an absorber in the focal plane above the reflector. The absorber circulates a heat transfer fluid that transfers heat to a power block. The system may include thermal storage.

Linear Fresnel Direct Steam
---------------------------

A :doc:`direct steam linear Fresnel <../csp-linear-fresnel-direct-steam/dslf_overview>` system consists of a field of slightly curved or flat Fresnel reflectors that focus light on an absorber in the focal plane above the reflector. Steam is used as the heat transfer fluid throughout the system. The system may not include thermal storage.

CSP Generic Model
-----------------

The :doc:`CSP generic model <../csp-generic-solar/gss_overview>` model allows you to model a system that consists of a solar field, power block with a conventional steam turbine, and optional thermal energy storage system. The model represents the solar field using a set of optical efficiency values for different sun angles and can be used for any solar technology that uses solar energy to generate steam for electric power generation.

Industrial Process Heat
~~~~~~~~~~~~~~~~~~~~~~~

The industrial process heat models are modified versions of CSP models with no power cycle that can be used to model thermal applications.

IPH Power Tower Molten Salt
---------------------------

The :doc:`IPH power tower molten salt <../iph-parabolic-trough/iph_trough>` model is a version of the CSP molten salt power tower model with no power cycle and modifications for process heat applications.

IPH Parabolic Trough Physical
-----------------------------

The :doc:`IPH parabolic trough <../iph-parabolic-trough/iph_trough>` model is a version of the CSP physical parabolic trough model with no power cycle and modifications for process heat applications.

IPH Linear Fresnel Direct Steam
-------------------------------

The :doc:`IPH linear direct steam <../iph-linear-fresnel-direct-steam/iph_linear_ds>` model is for a linear collector using direct steam with no power cycle and modifications for process heat applications.

IPH Linear Fresnel Molten Salt
------------------------------

The :doc:`IPH linear molten salt <../iph-linear-fresnel-direct-steam/iph_linear_ds>` model is for a linear collector using molten salt with no power cycle and modifications for process heat applications.

Marine Energy
~~~~~~~~~~~~~

Marine energy systems convert ocean wave or tidal energy to electrictiy.

Wave Energy
-----------

The :doc:`marine energy wave <../marine-energy-wave/me_wave>` model is for a system that uses a wave energy converter (WEC) to convert the energy of ocean waves into electricity

Tidal Energy
------------

The :doc:`marine energy tidal <../marine-energy-tidal/me_tidal>` model is for a system that uses a tidal energy converter (TEC) to convert the energy of ocean tides into electricity.

Wind
~~~~

The :doc:`wind power <../wind-power/wind_power>` model can model a single small or large wind turbine, or a project with two or more large or small wind turbines that sells power to the grid.

Fuel Cell - PV - Battery
~~~~~~~~~~~~~~~~~~~~~~~~

The :doc:`fuel cell-PV-battery <../fuel-cell/fuelcell>` model combines the PVWatts, battery, and a fuel cell model for systems that combine a photovoltaic array and fuel cell with an optional battery bank.

Solar Water Heating
~~~~~~~~~~~~~~~~~~~

SAM's :doc:`solar water heating <../solar-water-heating/swh_overview>` model represents a two-tank glycol system with an auxiliary electric heater and storage tank for residential or commercial applications. The model allows you to vary the location, hot water load profiles, and characteristics of the collector, heat exchanger, and solar tanks. 

Geothermal
~~~~~~~~~~

.. note:: SAM's geothermal power model is for electricity-generating systems, not ground source heat pumps or geo-exchange systems.

A :doc:`geothermal power plant <../geothermal/geo_overview>` uses heat from below the surface of the ground to drive a steam electric power generation plant. SAM analyzes the plant's performance over its lifetime, assuming that changes in the resource and electrical output occur monthly over a period of years, rather than over hours over a period of one year as in the solar and other technologies modeled by SAM.

Biomass Combustion
~~~~~~~~~~~~~~~~~~

A :doc:`biomass combustion system <../biopower/biopower>` burns a biomass feedstock (with or without supplementary coal) in a combustion system to generate steam that drives an electric power generation plant.

Custom Generation Profile
~~~~~~~~~~~~~~~~~~~~~~~~~

The :doc:`custom generation profile <../custom-generation/custom_generation>` model uses a generation profile as input to model any kind of power system. 

The custom generation profile model allows you to characterize the system's performance either using one of two options:

* Specify a nameplate capacity and capacity factor value: Assumes that the system generates power at a constant rate over the year.

* Specify an hourly our sub-hourly generation profile: Assumes that the system generates power according to the generation profile you specify.