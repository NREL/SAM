Fuel Cell
=========

Use the inputs on the Fuel Cell page to define the type and properties of the fuel cell and stack.

Fuel Cell Type
~~~~~~~~~~~~~~

The fuel cell type determines the type of fuel cell. For more detailed descriptions of the fuel cell types, see Chapter 2 of Steward, D. ; Penev, M.; Saur G.; Becker W. (2013) Fuel Cell Power Model Version 2: Startup Guide, System Designs, and Case Studies.

 NREL/TP- 5600-57457 (`PDF 4.5 MB) <https://www.nlr.gov/docs/fy13osti/57457.pdf>`__.

**Molten Carbonate Fuel Cell (MCFC)**
  Operate at temperatures of 650°C and above and can convert fuels to hydrogen without an external reformer. The high operating temperatures and corrosive electrolyte in MCFC fuel cells causes them to be less durable than other types. They also have longer ramp rates to maintain even distribution of temperature in the cell.

**Phosphoric Acid Fuel Cell (PAFC)**
  More mature cell type than the others and is the first to be used commercially. The operate at about 200°C with a lifetime of about 10 years.

**Solid Oxide Fuel Cells (SOFC)**
  Operate at temperatures between 650°C and 1,000°C and can convert a wide range of fossil fuels.

**Custom**
  Specify your own size, dynamic response and efficiency curves.

When you choose a fuel cell type from the list, SAM populates the following inputs with default values from the tables shown :ref:`below <defaultparameters>`. If you choose the Custom fuel cell type, replace the values of these inputs with values for your fuel cell type.

* Unit nameplate

* Minimum unit output

* Ramp rate up limit (per unit)

* Ramp rate down limit (per unit)

* Electrical and thermal efficiency curve

You can change the default values after applying them.

.. note:: Changing the fuel cell type does not change the degradation and replacement inputs, or costs on the :doc:`Installation costs <../installation-costs/cc_fuel_cell>` or :doc:`Operating costs <../operating-costs/oc_fuel_cell>` pages. Be sure to review all of the parameters to ensure that their values are appropriate for your analysis.

Cell and Stack Properties
~~~~~~~~~~~~~~~~~~~~~~~~~

The system properties define the size, dynamic response and degradation characteristics of the fuel cell and fuel cell stack. A cell stack is typically made up of several cells or units connected in series and parallel to achieve design voltage and power requirements.

.. note:: Because the fuel cell model is an energy flow model rather than an electrical model, it does not model current and voltages, so you do not need to specify how cells are configured in series and parallel.

Size
----

The size parameters determine the size and number of fuel cells in the cell stack.

**Unit nameplate, kW**
  The rated electric power capacity of a single fuel cell in AC kilowatts.

**Minimum unit output, % of nameplate or kW**
  The minimum electric power output of a single fuel cell either as a percentage of fuel cell unit rated power or in kW. 

**Number of units in stack**
  The number of fuel cells in the fuel cell stack.

**Total cell stack nameplate, kW**
  The rated capacity of the fuel cell cell stack in kilowatts.

*Total System Nameplate (kW) = Unit Nameplate (kW) × Number of Units*

**Minimum cell stack output, kW**
  The minimum electric power output of the entire cell stack in AC kilowatts. SAM will not allow the cell stack to deliver power at a level below this limit during operation.

*Minimum System Output (kW) = Minimum Unit Output (%) × Unit Nameplate (kW) × Number of Units*

  or

*Minimum System Output (kW) = Minimum Unit Output (kW) × Number of Units*

Dynamic Response
----------------

The dynamic response parameters determine how long it takes the cell stack to reach its rated power after startup or to respond to increases and decreases in electric load.

.. note:: SAM attempts to operate the cell stack at the operating capacity determined by the nameplate capacity and the parameters on the :doc:`Dispatch page <fuelcell_dispatch>`. The operating capacity is typically less than the nameplate capacity.

**Start simulation with fuel cell operating**
  Check this box if you want to start the simulation with the fuel cell stack at the operating capacity. If the box is not checked, the fuel cell will not start delivering heat and power at the beginning of the simulation until after the startup time is finished.

**Startup time, hours**
  The amount of time required in hours for the cell stack to reach its operating capacity after a shutdown, or at the beginning of the simulation if **Start simulation with fuel cell operating** is not checked.

**Shutdown time, hours**
  The amount of time required in hours for the cell stack power output to reach zero after a shutdown is initiated.

**Ramp rate up limit (per unit), %/hour or kW/hour**
  The maximum rate of increase in the cell stack power output, either as a percentage of nameplate capacity per hour or in kilowatts per hour. The cell stack power may be required to increase after a shutdown or in response to :doc:`dispatch <fuelcell_dispatch>`   requirements.

**Ramp rate down limit (per unit), %/hour or kW/hour**
  The maximum rate of decrease in the cell stack operating power output, either as a percentage of nameplate capacity per hour or in kilowatts per hour. The cell stack power may be required to decrease for a shutdown or in response to :doc:`dispatch <fuelcell_dispatch>`   requirements.

**Calculated ramp rate up limit (per unit), kW/hour**
  If you specify the ramp rate up limit in %/hour, the calculated value shows the rate in kW/hour.

  Ramp Rate Up Limit (kW/hour) = Unit Nameplate (kW) *× Ramp Rate Up Limit (%/hour)*

**Calculated ramp rate down limit (per unit), kW/hour**
  If you specify the ramp rate down limit in %/hour, the calculated value shows the rate in kW/hour.

  Ramp Rate Down Limit (kW/hour) = Unit Nameplate (kW) *× Ramp Rate Down Limit (%/hour)*

Degradation
-----------

The degradation parameters determine how the cell stack maximum power declines from year to year or after the cell stack is shut down and restarted.

**Degradation**
  The annual degradation rate either as a percentage of capacity (%/year) or in kilowatts of capacity (kW/year).

**Degradation due to restarting cell stack, kW**
  The degradation in cell stack maximum power caused by shutting down and restarting the stack.

.. note:: The fuel cell degradation is separate from the system degradation defined on the :doc:`Degradation <../degradation/degradation_dc>` page.

Shutdown Schedule
-----------------

The shutdown schedule determines when the cell stack is taken out of service for service, repairs, or replacement.

**Shutdown hour of year**
  The hour of year that the cell stack shutdown period begins. 

  If the **Simulation over one year option** is active on the :doc:`DC Degradation <../degradation/degradation_dc>`   page, the maximum value is 8,760. 

  If the lifetime and degradation option is **Simulation over analysis period**, the shutdown hour may exceed 8,760, and you should specify the shutdown hour over the entire lifetime.

**Hours shut down, hours**
  The length of the shutdown period in hours.

**No. of shutdowns**
  The number of shutdown periods in the project lifetime.

Stack Replacement
~~~~~~~~~~~~~~~~~

The stack replacement parameters determine the number and timing of cell stack replacements over the project lifetime. The project lifetime is the same as the analysis period on the Financial Parameters input page.

.. note:: If you choose to model cell stack replacements, be sure to set the replacement cost on the :doc:`Installation costs <../installation-costs/cc_fuel_cell>` page to the cost of replacing the cell stack.

**No replacements**
  Do not model cell stack replacement.

**Replace at specified capacity**
  Replace the cell stack when the fuel cell maximum power available reaches the specified capacity.

**Fuel cell stack replacement threshold, %**
  The percentage of the cell stack nameplate 

**Replace at specified schedule**
  Replace the cell stack on a pre-determined annual schedule.

  To set the schedule, click **Edit array** and type a 1 in the row of the table for each year that the fuel cell stack is replaced. Type a zero for remaining rows. The first row in the table represents Year 1 in the project cash flow, or the first year that the system operates.

**Fuel cell stack replacement threshold, %**
**Fuel cell replacement schedule**
Efficiency
~~~~~~~~~~

.. _defaultparameters:

Default Parameter Values
~~~~~~~~~~~~~~~~~~~~~~~~

The default parameters for the three fuel cell types are from the `Fuel Cell Power Model <https://research-hub.nlr.gov/en/publications/fuel-cell-power-model-version-2-startup-guide-system-designs-and-/>`__.

Default Size and Dynamic Response Parameters
--------------------------------------------

The percentages in the following table are percentage of nameplate capacity.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Parameter
     - MCFC
     - PAFC
     - SOFC
   * - Unit Nameplate
     - 300 kW
     - 400 kW
     - 200 kW
   * - Minimum unit output
     - 20%
     - 133%
     - 30%
   * - Ramp rate up limit
     - 17 kW/h
     - 100%/hr
     - 250%/hr
   * - Ramp rate down limit
     - 17 kW/h
     - 100%/hr
     - 250%/hr

Default Efficiency Curves
-------------------------

The default electrical and thermal efficiency curves for each fuel cell type are shown in the tables below.

**Molten Carbonate (MCFC)**

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Power (%)
     - Electrical Efficiency LHV (%)
     - Maximum Heat Recovery (%)
   * - 0.0
     - 3.0
     - 50
   * - 16.1
     - 19.0
     - 50
   * - 25.5
     - 22.8
     - 50
   * - 34.8
     - 28.5
     - 50
   * - 44.1
     - 33.9
     - 50
   * - 53.4
     - 38.4
     - 50
   * - 62.7
     - 43.0
     - 49
   * - 72.0
     - 45.3
     - 48
   * - 81.4
     - 47.2
     - 47
   * - 90.7
     - 47.0
     - 46
   * - 100
     - 46.1
     - 45

**Phosphoric Acid (PAFC)**

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Power (%)
     - Electrical Efficiency LHV (%)
     - Maximum Heat Recovery (%)
   * - 0.0
     - 10.5
     - 71.6
   * - 10.0
     - 20.3
     - 63.8
   * - 20.0
     - 26.9
     - 58.5
   * - 30.0
     - 31.4
     - 54.9
   * - 40.0
     - 34.1
     - 52.8
   * - 50.0
     - 35.9
     - 51.3
   * - 60.0
     - 36.9
     - 50.5
   * - 70.0
     - 37.5
     - 50.0
   * - 80.0
     - 37.7
     - 49.8
   * - 90.0
     - 37.7
     - 49.9
   * - 100.0
     - 37.4
     - 50.0

**Solid Oxide (SOFC)**

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Power (%)
     - Electrical Efficiency LHV (%)
     - Maximum Heat Recovery (%)
   * - 0.0
     - 3.0
     - 50
   * - 16.1
     - 21.0
     - 50
   * - 25.5
     - 25.2
     - 50
   * - 34.8
     - 31.5
     - 50
   * - 44.1
     - 37.3
     - 50
   * - 53.4
     - 42.6
     - 50
   * - 62.7
     - 47.4
     - 49
   * - 72.0
     - 49.9
     - 48
   * - 81.4
     - 52.0
     - 47
   * - 90.7
     - 51.8
     - 46
   * - 100
     - 50.7
     - 45