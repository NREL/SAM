System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

   When you change the value of the following input variables, you should optimize the field geometry on the :doc:`Heliostat Field <mspt_heliostat_field>` page to be sure that the solar field geometry (number and position of heliostats, tower height, and receiver height and aspect ratio) is appropriate for the new value: **Design point DNI**, **Solar multiple**, **Design turbine gross output**, **Cycle thermal efficiency**.

Heliostat Field
~~~~~~~~~~~~~~~

**Design-point DNI, W/m****\ :sup:`2`\**
  The direct normal irradiance (DNI) available at the design point.

  Increasing this value indicates that fewer heliostats are needed to achieve the reference condition power, while decreasing this value has the opposite effect. The design-point DNI value should represent the DNI at which your plant should achieve the specified thermal rating, including thermal and piping losses 

  For design-point calculations involving solar irradiance, SAM uses the design-point DNI value with the sun position at noon on the summer solstice (June 21 north of the equator, and December 21 south of the equator).

**Solar multiple**
  The solar multiple determines the receiver's nominal thermal power. It is the ratio of the receiver thermal power to the cycle thermal power. For a system with no storage, the solar multiple should be close to or equal to one.

**Receiver thermal power, MWt**
  The thermal power required at the receiver outlet for the power cycle to operate at its design point.

*Receiver Thermal Power (MWt) = Solar Multiple × Cycle Thermal Power (MWt)*

Tower and Receiver
~~~~~~~~~~~~~~~~~~

**HTF hot temperature, °C**
  The temperature of the hot heat transfer fluid at the receiver outlet when the power cycle operates at its design point.

**HTF cold temperature, °C**
  The temperature of the cold heat transfer fluid at the receiver inlet when the power cycle operates at its design point.

Thermal Storage
~~~~~~~~~~~~~~~

**Full load hours of storage, hours**
  The nominal thermal storage capacity expressed in hours at full load: The number of hours that the storage system can supply energy at the cycle's design point. 

  SAM displays the equivalent storage capacity in MWht on the :doc:`Installation costs <../installation-costs/cc_tower>`   page.

**Solar field hours of storage, hours**
  The nominal thermal storage capacity expressed in hours of the solar field's design thermal power output.

*Solar Field Hours of Storage = Full Load Hours of Storage ÷ Solar Multiple*

  Where Full Load Hours of Storage is on the :doc:`Thermal Storage <../electric-thermal-energy-storage/etes_thermal_storage>`   page.

Electric HTF Heater
~~~~~~~~~~~~~~~~~~~

The electric HTF heater is an optional component that supplies electric heat to the cold HTF. 

.. note:: The electric HTF heater option requires that you enable dispatch optimization on the :doc:`System Control <mspt_system_control>` page.

**Enable electric heater to charge cold HTF**
  Enable the electric HTF heater. See inputs on the :doc:`Electric HTF Heater <mspt_electric_htf_heater>`   page.

**Heater multiple**
  The heater multiple determines the heater's nominal thermal power as a multiple of the **Cycle thermal power**.

**Heater thermal power**
  The heater's thermal output under design conditions. SAM displays the heater thermal power on :doc:`Installation costs <../installation-costs/cc_etes>`   page.

*Heater Thermal Power (MWt) = Heater Multiple × Cycle Thermal Power (MWt)*

**Heater hours of storage**
  The nominal thermal storage capacity expressed in hours of the heater's design thermal output.

*Heater Hours of Storage = Thermal Storage Full Load Hours of Storage ÷ Heater Multiple*

Power Cycle
~~~~~~~~~~~

**Design turbine gross output, MWe**
  The power cycle's electrical output at the design point, not accounting for parasitic losses. This value together with the gross-to-net conversion factor and nominal cycle thermal efficiency determines the nominal cycle thermal power.

**Estimated gross-to-net conversion factor**
  An estimate of the ratio of the electric energy delivered to the grid to the power cycle gross output. The value represents the value of expected parasitic losses at the design point. SAM uses the factor to calculate the power cycle's nameplate capacity for capacity-related calculations, including the estimated total cost per net capacity value on the :doc:`Installation costs <../installation-costs/cc_tower>`   page, and the capacity factor reported in the results.

**Estimated net output at design (nameplate), MWe**
  The power cycle nominal electrical output.

*Estimated Net Output at Design (MWe) = Design Gross Output (MWe) × Estimated Gross-to-net Conversion Factor*

**Cycle thermal efficiency**
  The thermal-to-electric conversion efficiency of the power cycle at the design point. Note that this a nominal value: SAM calculates the power cycle's actual efficiency during the simulation and reports the actual value each time step in the :doc:`results <../getting-started/results_page>`  . This value together with the turbine gross output determines the cycle nominal thermal power.

**Cycle thermal power, MWt**
  The thermal power required at the power cycle inlet for it to operate at its design point.

*Cycle Thermal Power (MWt) = Design Turbine Gross Output (MWe) ÷ Cycle Thermal Efficiency*