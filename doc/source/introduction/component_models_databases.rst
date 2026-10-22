Models and Databases
====================

This topic lists all of SAM's performance models and describes the component-level models and databases SAM uses.

.. _systemperformance:

System Performance Models
~~~~~~~~~~~~~~~~~~~~~~~~~

The system models represent a complete renewable energy system and were developed by NLR using algorithms from partners listed below.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Model Name
     - Model Developer
   * - Biomass Combustion
     - NLR
   * - CSP Generic Model
     - NLR
   * - CSP Integrated Solar Combined Cycle
     - NLR
   * - CSP Linear Fresnel Direct Steam
     - NLR
   * - CSP Linear Fresnel Molten Salt
     - NLR
   * - CSP Parabolic Trough (Empirical)
     - NLR
   * - CSP Parabolic Trough (Physical)
     - NLR, University of Wisconsin
   * - CSP Power Tower Molten Salt
     - NLR, University of Wisconsin
   * - Energy Storage
     - NLR
   * - Fuel Cell
     - NLR
   * - Custom Generation Profile
     - NLR
   * - Geothermal
     - NLR, Princeton Energy Resources International
   * - High Concentration PV
     - NLR
   * - Marine Energy
     - NLR
   * - Photovoltaic (detailed)
     - NLR with component models from Sandia National Laboratories and the University of Wisconsin
   * - Photovoltaic (PVWatts)
     - NLR
   * - Process Heat Linear Fresnel Direct Steam
     - NLR
   * - Process Heat Parabolic Trough
     - NLR
   * - Solar Water Heating
     - NLR
   * - Wind
     - NLR

.. _componentperformance:

Component Performance Models
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The detailed photovoltaic and wind power models include options for choosing a component performance model to represent part of the system.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Model Name
     - Component
     - Developer
   * - Simple Efficiency Module Model
     - Photovoltaic module
     - NLR
   * - CEC Performance Model with Module Data base
     - Photovoltaic module
     - University of Wisconsin 
   * - CEC Performance Model with User Entered Specifications
     - Photovoltaic module
     - Adapted by NLR
   * - Sandia PV Array Performance Model with Module Database
     - Photovoltaic module
     - Sandia National Laboratories
   * - Single Point Efficiency Inverter
     - Inverter
     - NLR
   * - Sandia Performance Model for Grid Connected PV Inverters
     - Inverter
     - Sandia National Laboratories
   * - Wind Turbine Design Model
     - Wind Turbine
     - NLR
   * - Wind Power Curve Model
     - Wind Turbine
     - NLR

.. _componentdatabases:

Component Parameter Databases
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some of the component models use a library of input parameters to represent the performance characteristics of the component. The libraries listed below are owned by organizations other than NLR.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Library Name
     - Component
     - Owner
   * - CEC Modules
     - PV module
     - California Energy Commission
   * - Sandia Inverters
     - Inverter
     - Sandia National Laboratories
   * - Sandia Modules
     - PV module
     - Sandia National Laboratories

Online Financial Model Data
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _financialdata:

SAM can automatically download data from the following online databases to populate values on its financial model input pages.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Database Name
     - Type of Data
     - Database Manager
   * - `OpenEI U.S. Utility Rate Database <http://en.openei.org/wiki/Utility_Rate_Database>`__
     - Retail electricity prices and rate structures
     - NLR and Illinois State University

.. _weatherdata:

Online Renewable Resource and Weather Data Sources
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM can automatically download renewable energy resource and weather data from the following online databases.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Database Name
     - Type of Resource Data
     - Database Manager
   * - `National Solar Radiation Database <https://nsrdb.nlr.gov/>`__
     - Solar and Meteorological
     - NLR 
   * - `Wind Integration Datasets <https://www.nlr.gov/grid/wind-toolkit.html>`__
     - Wind and Meteorological
     - NLR with 3Tier and AWS Truepower

Solar and Wind Resource Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM comes with a small set of sample weather files for the solar and wind performance models. You can use the download tools on the Location and Resource page to download solar resource data and from the Wind Resource input page to download wind data.