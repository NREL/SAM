

The parameters on the Thermal Storage page describe the properties thermal energy storage system. Dispatch controls are on the System Control page.

The power tower storage model requires that the heat transfer fluid volume, tank loss coefficients, and tank temperatures be specified. SAM calculates the storage tank geometry to ensure that the storage system can supply energy to the power block at its design thermal input capacity for the number of hours specified by the Full Load TS Hours variable.

.. note:: Because the storage capacity is not tied to the solar multiple on the Heliostat Field page, be careful to choose a storage capacity that is reasonable given the system's thermal capacity. Mismatched storage and solar thermal capacities will result in high levelized cost of energy (LCOE) values.

System Design Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

The system design parameters are from the System Design page, where you can define the design-point parameters of the entire power tower system.

**Cycle thermal power, MWt**
  The thermal power required at the power cycle inlet for it to operate at its design point.

*Cycle Thermal Power (MWt) = Design Turbine Gross Output (MWe) ÷ Cycle Thermal Efficiency*

**Hours of storage at power cycle full load, hours**
  The nominal thermal storage capacity expressed in hours at full load: The number of hours that the storage system can supply energy at the cycle's design point. 

  SAM displays the equivalent storage capacity in MWht on the Installation Costs page.

**HTF hot temperature, °C**
  The  temperature of the hot heat transfer fluid at the receiver outlet when the power cycle operates at its design point.

**HTF cold temperature, °C**
  The temperature of the cold heat transfer fluid at the receiver inlet when the power cycle operates at its design point.

Two Tank Storage
~~~~~~~~~~~~~~~~

**TES thermal capacity**
  The nominal thermal capacity of the storage system.

*TES Thermal Capacity = Hours of Storage at Power Cycle Full Load × Cycle Thermal Input Power at Design*

**Available HTF volume, m³**
  The total volume of storage fluid in both storage tanks.

  SAM calculates the total heat transfer fluid volume in storage based on the storage hours at full load and the power block design turbine thermal input capacity. The total heat transfer fluid volume is divided among the total number of tanks so that all hot tanks contain the same volume of fluid, and all cold tanks contain the same volume of fluid.

**Tank height, m**
  The height of the cylinder-shaped heat transfer fluid volume in each tank. SAM calculates the height based on the diameter and storage volume of a single tank.

**Tank fluid minimum height, m**
  The minimum allowable height of fluid in the storage tank(s). The mechanical limits of the tank determine this value.

**Parallel tank pairs**
  The number of parallel hot-cold storage tank pairs. Increasing the number of tank-pairs also increases the volume of the heat transfer fluid exposed to the tank surface, which increases the total tank thermal losses. SAM divides the total heat transfer fluid volume among all of the tanks, and assumes that each hot tank contains an equal volume of fluid, and each cold tank contains and equal volume.

**Tank diameter, m**
  The diameter of the cylinder-shaped heat transfer fluid volume in each storage tank.

**Wetted loss coefficient, W/m²/K**
  The thermal loss coefficient that applies to the portion of the storage tank holding the storage heat transfer fluid.

**Estimated heat loss, MWt**
  Heat loss from the storage system at the design-point.

**Initial Hot HTF Percent, %**
  The fraction of the storage heat transfer fluid in the hot storage tank at the beginning of the simulation.

**Cold tank heater temperature set point, °C**
  The minimum allowed cold tank temperature. Whenever the heat transfer fluid temperature in storage drops below the set-point value, the system adds sufficient thermal energy from an electric tank heater to storage to reach the set-point.

**Cold tank heater capacity, MWe**
  The maximum electric load of the cold tank electric heater.

**Hot tank heater temperature set point, °C**
  The minimum allowed hot tank temperature. Whenever the heat transfer fluid temperature in storage drops below the set-point value, the system adds sufficient thermal energy from an electric tank heater to storage to reach the set-point.

**Hot tank heater capacity, MWe**
  The maximum electric load of the hot tank electric heater.

**Tank heater efficiency**
  The electric-to-thermal conversion efficiency of the hot tank and cold tank heaters.