System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

Design Point Parameters
~~~~~~~~~~~~~~~~~~~~~~~

Solar Field
-----------

**Boiling**
  The solar field delivers two-phase steam.

**Subcooled**
  The solar field delivers sub-cooled, pressurized water with single-phase liquid at the outlet.

**Field outlet steam quality**
  The field outlet steam quality is used to calculate the mass flow rate of steam. Steam quality is two-phase at outlet and completely condensed at inlet. This value represents the fraction of fluid exiting the solar field that is in vapor phase. The balance of the unevaporated fluid recirculates to the inlet of the solar field and is mixed with the subcooled feedwater from the process heat application.

**Subcooled dT, °C**
  Temperature difference for  the Subcooled option.

**Target outlet temperature, °C**
  Target outlet temperature calculated based on the boiling or subcooled option.

**Design-point DNI, W/m****\ :sup:`2`\**
  The direct normal irradiance (DNI) available at the design point.

  Increasing this value indicates that fewer heliostats are needed to achieve the reference condition power, while decreasing this value has the opposite effect. The design-point DNI value should represent the DNI at which your plant should achieve the specified thermal rating, including thermal and piping losses 

  For design-point calculations involving solar irradiance, SAM uses the design-point DNI value with the sun position at noon on the summer solstice (June 21 north of the equator, and December 21 south of the equator).

**Target Solar multiple**
  The field aperture area expressed as a multiple of the aperture area required to meet the thermal load.

**Target receiver thermal power, MWt**
  The thermal power required at the receiver outlet to meet the design thermal load.

*Receiver Thermal Power (MWt) = Solar Multiple × Heat Sink Power (MWt)*

This set of inputs defines the design-point operating conditions of the steam in the solar field. The field inlet and outlet temperatures, the pressure constraints, and the boiler outlet quality (if applicable) are used to calculate the enthalpy of steam during the annual performance calculation at each collector module in the loop.

**Field inlet temperature, °C**
  The estimated temperature of feedwater from the power cycle at the inlet of the solar field. This value is used to calculate estimated thermal losses from the solar field at design, and is not directly used in calculating the hourly performance for the annual simulation. The field inlet temperature is calculated during performance runs based on the power cycle conversion efficiency and the steam temperature at the inlet of the power cycle.

Heat Sink
---------

The heat sink parameters describe the process heat application's thermal load.

**Heat sink power (MWt)**
  Thermal input to the heat sink at design. Together with the target solar multiple, this value determines the receiver design conditions.

**Heat sink inlet pressure (bar)**
  The steam pressure at the heat sink inlet.

**Heat sink fractional pressure drop**
  The pressure drop across the heat sink.

System Availability
-------------------

.. include:: ../includes/snip_system_availability.rst

