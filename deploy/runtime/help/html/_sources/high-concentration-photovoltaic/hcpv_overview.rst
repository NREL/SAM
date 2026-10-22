High Concentration Photovoltaic
===============================

The high concentration photovoltaic (HCPV) model represents a HCPV system as an array of modules with one or more inverters as specified on the Array page. SAM models the HCPV module using a cell efficiency curve and a set of loss factors that you specify on the Module page. The multi-junction cell's efficiency curve is a linear interpolation of the table of power conversion efficiencies as a function of incident beam irradiance (direct normal irradiance, or DNI) that you specify. The model uses an air mass modifier polynomial to approximate spectral effects on the performance of the module. You can also specify loss factors to account for optical lens, alignment error, tracker error, wind flutter, and other CPV-specific losses. The HCPV uses SAM's Sandia inverter model.

Basic Steps
~~~~~~~~~~~

1. On the Location and Resource page, choose a weather file to represent the solar resource at the project location
-------------------------------------------------------------------------------------------------------------------

  SAM offers three weather data options. You can:

* Choose a location from the list at or near your project site. SAM will simulate the system using a file from SAM's solar resource library, which includes data from the National Solar Radiation DatabaseNLR's TMY2 database.

* Download a weather file for your site from an online database. SAM will simulate the system using a file from NLR's database of satellite-derived solar resource data.

* Download a file from NLR's TMY3 database.

* Create a weather file in TMY3 format with your own data

For preliminary analyses in the United States either use a TMY2 file if there is a file in the database with similar weather to the project site, or download a file for the location.

For more robust analysis, download TMY3 data, or use your own data. You may also want to analyze your project using weather data from different sources to develop an understanding of how uncertainty in the weather data affects the metrics of interest for your project.

4. Specify the system's characteristics
---------------------------------------

#. On the :doc:`Inverter <hcpv_inverter>` page, choose an inverter.

#. On the :doc:`Module <hcpv_module>` page, specify the CPV module parameters.

#. On the :doc:`Array <hcpv_array>` page, specify the system's size and other parameters.

5. On the Installation costs and Operating costs pages, specify the project costs
---------------------------------------------------------------------------------

The :doc:`installation costs <../installation-costs/cc_pv>` apply to year zero of the cash flow. 

The :doc:`operating costs <../operating-costs/oc_operating>` apply in years one and later of the cash flow.

6. Run a simulation and review results
--------------------------------------

.. image:: ../images/SS_MainWindow-RunAllSimulationsButton.png
   :align: center
   :alt: SS_MainWindow-RunAllSimulationsButton.png

See :doc:`Results Page <../getting-started/results_page>` for details.