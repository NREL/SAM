System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

Design Point Parameters
~~~~~~~~~~~~~~~~~~~~~~~

Solar Field
-----------

**Design-point DNI, W/m****\ :sup:`2`\**
  The direct normal irradiance (DNI) available at the design point.

  Increasing this value indicates that fewer heliostats are needed to achieve the reference condition power, while decreasing this value has the opposite effect. The design-point DNI value should represent the DNI at which your plant should achieve the specified thermal rating, including thermal and piping losses 

  For design-point calculations involving solar irradiance, SAM uses the design-point DNI value with the sun position at noon on the summer solstice (June 21 north of the equator, and December 21 south of the equator).

**Target solar multiple**
  The design ratio of the target receiver thermal power and heat sink power. This value can be used to oversize the receiver design output relative to the heat sink.

**Target receiver thermal power, MWt**
  The thermal power required at the receiver outlet to meet the design thermal load.

*Receiver Thermal Power (MWt) = Solar Multiple × Heat Sink Power (MWt)*

**Loop inlet HTF temperature (ºC)**
  The temperature of HTF at the loop inlet under design conditions. The actual temperature during operation may differ from this value.

**Loop outlet HTF temperature (ºC)**
  The temperature of the HTF at the outlet of the loop under design conditions. During operation, the actual value may differ from this set point. This value represents the target temperature for control of the HTF flow through the solar field and will be maintained when possible.

Heat Sink
---------

The heat sink parameters describe the process heat application's thermal load.

**Heat sink power (MWt)**
  Thermal input to the heat sink at design. Together with the target solar multiple, this value determines the receiver design conditions. For future versions of this model that may contain thermal energy storage, this parameter will guide the interaction between the heat sink and thermal energy storage.

**Pumping power for HTF through heat sink (kW/kg/s)**
  The electricity consumption per unit of mass flow rate associated with pumping the HTF through the heat sink.

**Choose Number of Loops**
  This macro allows the user to specify the number of loops in the simulation. The macro then calculates the corresponding heat sink power and gives the user the option to set it as the new input.

Thermal Storage
---------------

**Hours of storage at design point, hours**
  The nominal thermal storage capacity expressed in hours at full load: The number of hours that the storage system can supply energy at the design point.

System Availability
-------------------

.. include:: ../includes/snip_system_availability.rst

System Summary
~~~~~~~~~~~~~~

**Actual number of loops**
  The actual number of loops in the field, equal to the solar multiple times the required number of loops at a solar multiple of 1.0. The required number of loops is rounded to the nearest integer to represent a realistic field layout.

**Total aperture reflective area (m²)**
  The actual aperture area based on the actual number of loops in the field, equal to the single loop aperture times the actual number of loops.

**Actual solar multiple**
  For Option 1 (solar multiple mode), the calculated solar multiple based on the actual (rounded) number of loops in the field. For Option 2 (field aperture mode), the solar multiple value corresponding to the thermal output of the field based at design point: The total aperture reflective area divided by the field thermal output. 

**Actual field thermal output (MWt)**
  The thermal energy delivered by the solar field under design conditions at the actual solar multiple.