System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

   When you change the value of the following input variables, you should optimize the field geometry on the :doc:`Heliostat Field <iph_mspt_heliostat_field>` page to be sure that the solar field geometry (number and position of heliostats, tower height, and receiver height and aspect ratio) is appropriate for the new value: **Design point DNI**, **Solar multiple**, **Design turbine gross output**, **Cycle thermal efficiency**.

Heliostat Field
~~~~~~~~~~~~~~~

**Design-point DNI, W/m****\ :sup:`2`\**
  The direct normal irradiance (DNI) available at the design point.

  Increasing this value indicates that fewer heliostats are needed to achieve the reference condition power, while decreasing this value has the opposite effect. The design-point DNI value should represent the DNI at which your plant should achieve the specified thermal rating, including thermal and piping losses 

  For design-point calculations involving solar irradiance, SAM uses the design-point DNI value with the sun position at noon on the summer solstice (June 21 north of the equator, and December 21 south of the equator).

**Solar multiple**
  The :ref:`solar multiple <iph-mspt-solarmultiple>`   determines the receiver's nominal thermal power. It is the ratio of the receiver thermal power to the cycle thermal power. For a system with no storage, the solar multiple should be close to or equal to one.

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

  Where Full Load Hours of Storage is on the :doc:`Thermal Storage <iph_mspt_thermal_storage>`   page.

Heat Sink
~~~~~~~~~

The heat sink parameters describe the process heat application's thermal load.

**Heat sink thermal power, MWt**
  Thermal input to the heat sink at design. Together with the target solar multiple, this value determines the receiver design conditions.

**Pumping power for HTF through power block, kW/kg/s**
  Electrical power required to circulate the heat transfer fluid.

.. _iph-mspt-solarmultiple:

Solar Multiple
~~~~~~~~~~~~~~

.. include:: ../includes/snip_solar_multiple.rst

