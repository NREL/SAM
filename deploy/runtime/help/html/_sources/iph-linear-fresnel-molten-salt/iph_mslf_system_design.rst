System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

Solar Field
-----------

The solar field design parameters determine the size of the solar field. See the :doc:`Solar Field <iph_mslf_solar_field>`, and :doc:`Collector and Receiver <iph_mslf_collector_receiver>` pages to set detailed parameters for the field.

**Option 1 and Option 2**
  For Option 1 (solar multiple mode), SAM calculates the field aperture based on the value you enter for the solar multiple. 

  For option 2 (field aperture mode), SAM calculates the solar multiple based on the field aperture value you enter. Note that SAM does not use the value that appears dimmed for the inactive option. See :ref:`Solar Multiple <iph-mslf-solarmultiple>`   for details.

**Solar Multiple**
  The field aperture area expressed as a multiple of the aperture area required to operate the power cycle at its design capacity. See :ref:`Solar Multiple <iph-mslf-solarmultiple>`   for details.

**Field Aperture, m²**
  The total solar energy collection area of the solar field in square meters. Note that this is less than the total mirror surface area.

.. note:: For simulations, SAM uses the calculated solar multiple and field aperture values shown in blue to the right of the inputs. The calculated value is likely to be different from the value you see on the System Design page because the field aperture changes in discrete steps based on the number of loops.

**Design point DNI, W/m²**
  The design point direct normal radiation value, used in solar multiple mode to calculate the aperture area required to drive the heat sink cycle at its design capacity. Also used to calculate the design mass flow rate of the heat transfer fluid for header pipe sizing.

**Actual field thermal power, MWt**
  The field thermal power output at design. The loop mass flow limit on the Solar Field page may limit the mass flow rate to less than design. The actual field thermal power represents the design power constrained by the mass flow rate limit.

**Field thermal power w/ no defocus constraints**
  Ideal field thermal power with no mass flow constraints

*Ideal Thermal Power (MWt) = Solar Multiple × Cycle Thermal Power (MWt)*

**Loop inlet HTF temperature, ºC**
  The temperature of HTF at the loop inlet under design conditions. The actual temperature during operation may differ from this value. SAM sets the power cycle's design outlet temperature equal to this value.

**Loop outlet HTF temperature, ºC**
  The temperature of the HTF at the outlet of the loop under design conditions. During operation, the actual value may differ from this set point. This value represents the target temperature for control of the HTF flow through the solar field and will be maintained when possible.

**Number of loops**
  The number of loops in the field, equal to the solar multiple times the required number of loops at a solar multiple of 1.0. The required number of loops is rounded to the nearest integer to represent a realistic field layout.

Heat Sink
---------

The heat sink parameters describe the process heat application's thermal load.

**Heat sink thermal power, MWt**
  Thermal input to the heat sink at design. Together with the target solar multiple, this value determines the receiver design conditions.

**Heat sink thermal power, MMBtu/hr**
  The heat sink thermal power expressed in MMBtu/hr. This is the value used to calculate the capacity based incentive if one is specified on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.

**Pumping power for HTF through power block, kW/kg/s**
  Electrical power required to circulate the heat transfer fluid.

Thermal Energy Storage
----------------------

The thermal storage design parameters determine the size of the thermal energy storage system (TES). See the :doc:`Thermal Storage <iph_mslf_thermal_storage>` page for detailed TES parameters.

**Hours of storage at design point, hours**
  The thermal storage capacity expressed in number of hours of thermal energy delivered at the design power cycle thermal power. The physical capacity is the number of hours of storage multiplied by the power cycle design thermal input. Used to calculate the thermal energy system's maximum storage capacity.

.. _iph-mslf-solarmultiple:

Solar Multiple
~~~~~~~~~~~~~~

.. include:: ../includes/snip_solar_multiple.rst

