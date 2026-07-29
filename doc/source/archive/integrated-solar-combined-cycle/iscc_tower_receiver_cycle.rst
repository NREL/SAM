Tower Receiver Cycle
====================

External Receiver
~~~~~~~~~~~~~~~~~

**Receiver height, m**
  Height in meters of the receiver panels.

**Receiver diameter, m**
  Total diameter in meters of the receiver. The distance from center of the receiver to center of a receiver panel. The width of a single panel is the circumference of receiver divided by number of panels.

**Number of panels**
  Number of vertical panels in the receiver.

.. note:: For **Flow Pattern** options 1-4, **Number of Panels** must be a multiple of 2. If you specify an invalid number, the simulation will fail, and SAM will generate an error message.

Solar Field Design Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Solar multiple**
  The solar multiple determines the receiver's nominal thermal power. It is the ratio of the receiver thermal power to the cycle thermal power. For a system with no storage, the solar multiple should be close to or equal to one.

**Design-point DNI, W/m****\ :sup:`2`\**
  The direct normal irradiance (DNI) available at the design point.

  Increasing this value indicates that fewer heliostats are needed to achieve the reference condition power, while decreasing this value has the opposite effect. The design-point DNI value should represent the DNI at which your plant should achieve the specified thermal rating, including thermal and piping losses 

  For design-point calculations involving solar irradiance, SAM uses the design-point DNI value with the sun position at noon on the summer solstice (June 21 north of the equator, and December 21 south of the equator).

Receiver Heat Transfer Properties
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Tube outer diameter, mm**
  The outer diameter in millimeters of the tubing that carries the heat transfer fluid through the receiver panels. Typical values range from 25 mm to 50 mm.

**Tube wall thickness, mm**
  The thickness in millimeters of the individual receiver panel tube walls.

**Coating emittance**
  The emissivity of the receiver coating, assumed to be black-body emissivity constant over the range of wavelengths.

**Coating absorptance**
  Absorptance fraction of receiver tube coating. Typical values are 0.91 to 0.95.

**Heat loss factor**
  A receiver heat loss adjustment factor that can be used when the calculated heat loss value deviates from an expected value. The default value is 1, corresponding to no heat loss correction. The calculated receiver heat loss is the sum of convection and radiation losses from the receiver, reported in the hourly results as Rec_conv_loss and Rec_rad_loss, respectively.

Design Operation
~~~~~~~~~~~~~~~~

**Minimum receiver turndown fraction**
  The minimum allowable fraction of the maximum flow rate to receiver.

**Maximum receiver operation fraction**
  The maximum allowable fraction of the maximum flow rate to receiver. SAM removes heliostats from operation if the HTF mass flow rate exceeds the maximum allowable value.

**Receiver startup delay time**
  The time in hours required to start the receiver. The receiver starts whenever the radiation incident on the field in the previous hour is zero, and there is sufficient thermal energy in the current hour to meet the thermal design requirement. SAM calculates the start up energy as the product of the available thermal energy, startup delay time, and startup delay energy fraction.

**Receiver startup delay energy fraction**
  Fraction of receiver design thermal power required by the receiver during the startup period.

**Receive HTF pump efficiency**
  The electro-mechanical efficiency of the receiver heat transfer fluid pump.

**Maximum flow rate to teceiver, kg/s**
  The maximum heat transfer fluid flow rate at the receiver inlet. SAM calculates this value as a function of the maximum heat transfer fluid velocity in the receiver.

*Maximum Flow Rate to Receiver = Maximum Receiver Operation Fraction × Receiver Thermal Power (MWt) × 1,000,000*
 *÷ HTF Specific Heat × 1,000,000 × ( HTF Hot Temperature ºC - HTF Cold Temperature ºC  )*

Materials and Flow
~~~~~~~~~~~~~~~~~~

**HTF type**
  One of two types of solar salt used for the heat transfer fluid, also called the working fluid. You can also add a user defined HTF by choosing the user defined option and clicking the **Edit** button to open the HTF properties editor.

**Property table for user-defined HTF**
  When the HTF type is "user defined," the Edit button provides access to the HTF properties editor.

**Material type**
  The material of the receiver panel tubes, typically a stainless-steel alloy. The current version of SAM allows only one material type.

**Flow pattern**
  One of eight available heat transfer fluid flow configurations shown in the diagram. 

  The view in the diagram is from the top of the receiver, assuming that panels are arranged in a circle around the center of the receiver. Arrows show the direction of heat transfer fluid flow into, through, and out of the receiver, with arrows pointing up indicating flow toward the north.

.. note:: For **Flow Pattern** options 1-4, **Number of Panels** must be a multiple of 2. If you specify an invalid number, the simulation will fail, and SAM will generate an error message.

Receiver Flux Modeling Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Maximum receiver flux, kWt/m****\ :sup:`2`\**
  The maximum allowable incident flux on the receiver, before reflection, re-radiation, or convection losses. 

  The maximum flux is used only during optimization when SAM manipulates the receiver geometry to satisfy the maximum flux constraint. Flux intensity is determined at the reference design condition (noon on the summer solstice) at the Design-point DNI value that you specify. The flux is checked along a grid of points on the receiver. The flux grid has 10 equally-spaced vertical points per panel.

**Estimated receiver heat loss, kWt/m****\ :sup:`2`\**
  The estimated receiver heat loss at design-point operation per unit area of receiver surface. 

  This value is used to determine the amount of additional power required from the solar field to overcome receiver heat loss. In previous versions of SAM, this value was assumed to be 30 kWt/m2. 

  Note that this value is used only to determine the required solar field size and does not affect annual hourly performance simulation results once the layout has been specified.

**Receiver flux map resolution**
  A calculated value indicating the number of flux points on the receiver considered during flux mapping. The number of flux points is equal to the number of panels on the receiver, but must be at least 12.

**Number of days in flux map lookup**
  The number of days used to calculate the receiver flux map lookup table. 

  When calculating receiver flux maps, SAM uses the number of days that you specify but spaces them throughout the year according to the variation in sun position. SAM chooses the actual days to simulate by maintaining equal solar angle spacing among each day. The spacing between days is important in ensuring that the flux map lookup table is as comprehensive as possible. 

  The following plot shows flux map sun positions for 8 lookup table days and hourly simulations. Equal spacing is maintained between daily profiles. 

  .. image:: ../images/IMG_TR_flux-map.png
     :align: center
     :alt: IMG_TR_flux-map.png

**Hourly frequency in flux map lookup**
  The hourly frequency over the course of each flux map simulation day at which simulations are executed. 

  For example, the plot above (see No. Days in Flux Map Lookup) provides simulations on an hourly frequency. If you specify 2 as the hourly frequency, simulations will be executed every other hour; 3 will be executed every third hour, etc. 

  SAM always constructs the hourly simulations to ensure symmetry about solar noon. A simulation will always be performed at solar noon, and the hourly frequency determines the nearest times for simulation in the morning and afternoon hours. Increasing the hourly frequency value decreases the number of simulations included in the flux map lookup table and may increase interpolation error. Decreasing the frequency has the opposite effect.

Tower Height and Piping Losses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Tower height, m**
  Height in meters of the tower structure, equal to the vertical distance between the heliostat pivot points and the vertical center of receiver.

**Piping heat loss coefficient, Wt/m**
  Thermal energy loss per meter length of piping between the tower and thermal storage system, including both hot (downcomer) and cold (riser) header piping. This loss does not include thermal losses from the receiver due to convection, emission, or reflection. 

**Piping length constant, m**
  Fixed piping length used to calculate the total piping length for thermal losses. 

**Piping length multiplier**
  Factor multiplying the tower height that is used to calculate the total piping length for thermal losses. 

**Piping length, m**
  Calculated total piping length used to calculate the piping thermal losses.

*Piping Length (m) = Piping Length Multiplier × Tower Height (m) + Piping Length Constant*

  Note that this this piping length is used only in the calculation of thermal energy loss from the receiver and is not used for pressure loss or pumping parasitic power requirement calculations. 

**Total piping loss, kWt**
  The calculated thermal energy loss due to header piping, applied whenever the receiver is operating. 

*Total Piping Loss (kWt) = Piping Length (m) × Piping Heat Loss Coefficient (Wt/m) ÷ 1000 (kW/W)*

DESIGN POINT Integration with Natural Gas Combined Cycle
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Default values of these parameters are from the NLR Triple Pressure NGCC model developed in IPSEpro using general information to define components. As the size of the duct burner increases, the efficiency of the NGCC decreases, but the value of replacing the duct burner with solar thermal power increases. Consequently, a thorough economic analysis of an ISCC plant must necessarily consider the capital and operational costs of the NGCC plant, and that capability is beyond the current scope of the SAM model.

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst

