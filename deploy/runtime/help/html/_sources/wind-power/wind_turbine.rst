Wind Turbine
============

The wind turbine parameters specify the turbine power curve and hub height of a single turbine.

For a project with multiple turbines, SAM assumes that the :doc:`wind farm <wind_farm>` consists of identical turbines.

For a technical description of the wind turbine models, see Chapters 5-7 of Freeman, J.; Gilman, P.; Jorgenson, J.; Ferguson, T. (2014). "Reference Manual for the System Advisor Model's Wind Performance Model." National Renewable Energy Laboratory, NREL/TP-6A20-60570. (`PDF 738 KB <https://docs.nlr.gov/docs/fy14osti/60570.pdf>`__)

For information about small wind certification, see:

* `Small Wind Certification Council <https://smallwindcertification.org/certified-small-turbines/>`__


* `IREC Certified Wind Turbines <https://irecusa.org/workforce-development/credentials-qa/certified-wind-turbines/>`__


SAM offers two options to specify the turbine parameters:

**Select a Turbine from the library**
  Choose a turbine from SAM's wind turbine library. SAM automatically populates the power curve, **Rated Output** and **Rotor Diameter** values from the library. You cannot change these values.

  Use this option when you want to model a project that uses commercially available wind turbines.

.. note:: If you have power curve data for a turbine that is not in the library, you can use a text editor or spreadsheet program to add it to the library. The library file is Wind Turbines.csv in the libraries folder of your :ref:`SAM installation folder <installationfolder>`.

**Define the turbine characteristics**
  Use this option when you are investigating different turbine design parameters.

For either option, you specify the turbine hub height and shear coefficient:

**Hub Height**
  The height of the center of the rotor above the ground.

.. note:: If you specify a wind resource file on the :doc:`Wind Resource <wind_resource>` page, and the difference between the hub height and the nearest wind speed data height in the file is greater than 35 meters, SAM stops the simulation and generates an error message.

**Shear Coefficient**
  The shear coefficient is a measure of the variation in wind speed with height above the ground at the turbine installation site. The default value of 0.14 (1/7) is a common assumption for the value in wind resource studies on land, and 0.11 may be appropriate over water for offshore wind farms.

  How and whether SAM uses the shear coefficient to estimate the wind speed at the turbine's hub height depends on the option you choose on the :doc:`Wind Resource <wind_resource>`   page:

*   For the **Wind Resource by Location** option (wind data file with time series data), SAM ignores the shear coefficient unless the file contains wind speed data at only a single height, or when the turbine hub height is either below the lowest height or above the highest height in the file.

*   For the **Wind Resource Characteristics** option (specify a Weibull distribution), SAM uses the shear coefficient with the power law, assuming that the measured wind speed height is 50 meters above the ground.

  See :ref:`Wind Shear and Power Curve Adjustments <shear>`   for an explanation of how SAM uses this value.

Wind Turbine Design Characteristics
-----------------------------------

The wind turbine design parameters are the input variables that are active when you choose the **Define the turbine design characteristics** option.

For a description of how SAM uses these inputs to calculate the turbine's power curve, see Section 5.2 of Freeman, J.; Gilman, P.; Jorgenson, J.; Ferguson, T. (2014). "Reference Manual for the System Advisor Model's Wind Performance Model." National Renewable Energy Laboratory, NREL/TP-6A20-60570. (`PDF 738 KB <https://docs.nlr.gov/docs/fy14osti/60570.pdf>`__)

**User-defined Rated Output**
  The turbine's nameplate capacity in kW.

**User-defined Rotor Diameter**
  The turbine's rotor diameter in meters.

**Maximum Cp**
  The rotor's power efficiency.

  The fraction of the rotor's total available power that the blades can convert to mechanical power. The theoretical maximum is the Betz limit of 0.59.

**Maximum tip speed**
  The maximum velocity of the blade tip.

**Maximum tip-speed Ratio**
  The maximum ratio of the blade tip speed to wind speed.

**Cut-in wind speed**
  The minimum wind speed at which the turbine generates electricity.

**Cut-out wind speed**
  The maximum wind speed at which the turbine generates electricity.

**Turbine elevation (above sea level)**
  The elevation of the ground at the base of the turbine's tower.

**Drive Train Design**
  Choose an option from the four available designs.

**Blade Design**
  Choose an option from the two available designs.

**Tower Design**
  Choose an option from the two available designs.