Parasitics
==========

The parameters on the Parasitics page describe parasitic electrical loads and other losses in the power tower system.

The parasitic loss variables are factors that SAM uses to calculate the estimated total parasitic loss and hourly parasitic losses, which are described in more detail below.

SAM calculates two types parasitic loss values. The first is an estimate of the total parasitic losses used to calculate the power block design thermal input, and the second are the hourly values calculated during simulation of the system's performance.

.. note:: Parasitic losses from components that do not exist in the system should be set to zero.

Parasitic Energy Consumption
----------------------------

**Startup energy of a single heliostat, kWe-hr**
  The electric energy in kilowatt-hours required to move each heliostat into position. Applies during hours when the heliostat is starting up.

**Tracking power for a single heliostat, kWe**
  The electric power in kilowatts required by the tracking mechanism of each heliostat in the field during hours of operation.

**Feedwater to boiler pump efficiency**
  The electro-mechanical efficiency of the pump.

**Fraction of rated gross power consumed at all times**
  The fraction of design-point gross power output from the power cycle that is used for parasitic losses associated with facility operation, HVAC, control, lighting, etc.

**Piping loss coefficient, Wt/m**
  Thermal loss per meter of piping as calculated on the Parasitics page. The total piping length is multiplied by the Piping loss coefficient to determine the thermal losses from piping that are incurred during solar field operation.

**Piping length constant, m**
  A constant piping length independent of the tower height that contributes to the total piping length value.

**Piping length multiplier**
  A value that multiplies the tower height from the Tower and Receiver page to help determine the total piping length for thermal losses. The multiplier only applies to the tower height and does not multiply the Piping length constant on the Parasitics page.

**Total piping length, m**
  Length of piping throughout the system: From the receiver to power block, power block to process heat, etc. The piping loss varies with output produced by turbine. The Total piping length is calculated as follows:

  .. image:: ../images/EQ_TowerDS_Parasitics.png
     :align: center
     :alt: EQ_TowerDS_Parasitics.png

  where *H*\ :sub:`Tower`\    is the tower height, *F*\ :sub:`p,mult`\    is the piping length multiplier, and *L*\ :sub:`p,const`\    is the piping length constant.

**Balance of plant parasitic, MWe/MWcap**
  Losses as a fraction of the power cycle electrical power output that apply in hours when the power block operates.

**Aux heater, boiler parasitic, MWe/MWcap**
  Parasitic power consumption incurred during operation of the backup fossil boiler, as a function of thermal power production of the fossil system. This parasitic is only applicable for systems with active fossil backup, and applies during hours in which the fossil system produces thermal power.