Parasitics
==========

To view the Parasitics page, click **Parasitics** on the main window's navigation menu. Note that for the empirical trough input pages to be available, the technology option in the :doc:`Technology and Market <../getting-started/choose_models>` window must be Concentrating Solar Power - Empirical Trough System.

The Parasitics page displays parameters describing losses due to parasitic electrical loads, such as drive motors, electronic circuits, and pump motors. SAM includes a set of default parasitic parameters for a range of solar trough power systems. Choose a reference parasitic system option that is the same or similar to the system you are modeling. SAM will automatically adjust the total parasitic load to match the size of the solar field and power block in the system you are modeling.

The design point parasitic values are the maximum possible values for each parasitic loss category. SAM calculates the hourly parasitic loss value for each category during simulation based on the design point, the PF and F0-F2 coefficients, and the solar field thermal output and power block load in each hour, and reports them in the :doc:`time series <../results/timeseries>` simulation results. The calculated parasitic loss values are never as high as the total design point parasitic losses.

For a more detailed description of the model, please download the CSP trough reference manual from the `SAM website <https://sam.nlr.gov/concentrating-solar-power/csp-publications>`__.

The values of input variables on the Parasitics page are stored in a library of reference solar fields. You can change the parameter values without changing the values stored in the library. For information about libraries, See :doc:`Working with Libraries <../reference/libraries>`.

Parasitic Electric Energy Use
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Current reference parasitic system**
  The reference system from the CSP trough parasticis library. SAM stores a set of parasitic parameters for reference systems. 

**Solar Field Area (m****\ :sup:`2`\****)**
  The calculated solar field area from the :doc:`Solar Field page <troughempirical_solar_field>`  . Used to calculate parasitic losses that are based on the solar field size with units of MWe/m  \ :sup:`2`\   .

**Gross Turbine Output (MWe)**
  The design turbine gross output value from the :doc:`Power Block page <troughempirical_power_block>`  . Used to calculate parasitic losses that are based on the power block capacity with units of MWe/Mwe.

**SCA Drives and Electronics (MWe)**
  Electrical losses from electric or hydraulic SCA drives that position the collector to track the sun and from electronic SCA tracking controllers and alarm monitoring devices. For hours when the solar field is operating, SAM calculates the loss as the product of the value you specify and the solar field area. For hours when the solar field is not operating, the value of the loss is zero.

.. note:: SAM does not use the "PF" value for the SCA Drives and Electronics loss calculation.

**Solar Field HTF Pumps**
  Electrical losses from cold HTF pumping in the solar field. Calculated as a function of the solar field area. These losses are calculated only in hours when the solar field is operating, which is defined as when the solar field load is greater than zero.

  .. image:: ../images/EQ_TRE_Par-HTFPump.png
     :align: center
     :alt: EQ_TRE_Par-HTFPump.png

**TES Pumps**
  Electrical losses from pumps in the TES system. Calculated as a function of the design turbine gross output.

**Antifreeze Pumping (MWe)**
  Electrical losses from HTF pumps in the solar field. Calculated as a function of the solar field area, calculated as a fraction of the solar field HTF pumps design point parasitic loss. These losses are used only in hours when the solar field is not operating, which is defined as when the solar field load is zero. 

**Power Block Fixed (MWe)**
  These fixed losses apply 24 hours per day, for all of the 8,760 hours of the year.

**Balance of Plant (MWe)**
  Electrical losses that apply in hours when the power block operates at part or full load.

**Heater and Boiler (MWe)**
  Losses that apply only when the back-up boiler is in operation.

**Cooling Towers (MWe)**
  The cooling tower parasitic losses are electrical losses that occur when the power block operates at part or full load. Calculated either as a function of power block load or at a fixed 50% or 100% of the design cooling tower parasitic losses.

**Cooling Tower Operation Mode**
  Determines how cooling tower parasitic losses are calculated. For "Cooling Tower at 50% or 100%," parasitic losses are calculated as 50% of the design cooling tower parasitic losses when the power block load is 0.5 or less, and as or 100% of the design parasitic losses when the power block load is greater than 0.5. For "Cooling Tower parasitics a function of load," cooling tower parasitic losses are calculated as a function of power block load.

**Total Design Parasitics (MWe)**
  The sum of collector drives and electronics, solar field HTF pump, night circulation pumping, power block fixed, balance of plant, heater/boiler, and cooling towers design loss values. This value represents the maximum possible value if all parasitic losses were to occur simultaneously in a given hour, and is typically greater than the actual parasitic losses. SAM displays the value for reference only, and does not use it in simulation calculations.

Each parasitic loss type has a set of parameters that includes a factor, PF and F0, F1, and F2 coefficient. The design point values are maximum values and are calculated using the factor and PF coefficient. SAM uses the F0-F2 coefficients in calculations for the hourly simulations, which are described in the reference manual.

Table . Design point parasitic loss equations for each parasitic loss category.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Source of Parasitic Loss
     - Equation
   * - SCA Drives and Electronics
     - *Factor x PF x Solar Field Area*
   * - Solar Field HTF Pumps
     - *Factor x PF x Solar Field Area*
   * - TES Pumps
     - *Factor x PF x Gross Turbine Output*
   * - Antifreeze Pumping
     - *Factor x Solar Field HTF Pump losses*
   * - Power Block Fixed
     - *Factor x Gross Turbine Output*
   * - Balance of Plant
     - *Factor x PF x Gross Turbine Output*
   * - Heater and Boiler
     - *Factor x PF x Gross Turbine Output*
   * - Cooling Towers
     - *Factor x PF x Gross Turbine Output*

The Total Design Point Parasitics is the sum of the design point parasitic loss categories:

* SCA Drives and Electronics

* Solar Field HTF Pumps

* TES Pumps

* Power Block Fixed

* Balance of Plant

* Heater and Boiler

* Cooling Towers