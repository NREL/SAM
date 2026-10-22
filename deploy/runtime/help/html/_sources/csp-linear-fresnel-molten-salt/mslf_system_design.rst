System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

Solar Field
-----------

The solar field design parameters determine the size of the solar field. See the :doc:`Solar Field <mslf_solar_field>`, and :doc:`Collector and Receiver <mslf_collector_and_receiver>` pages to set detailed parameters for the field.

**Option 1 and Option 2**
  For Option 1 (solar multiple mode), SAM calculates the field aperture based on the value you enter for the solar multiple. 

  For option 2 (field aperture mode), SAM calculates the solar multiple based on the field aperture value you enter. Note that SAM does not use the value that appears dimmed for the inactive option. See :ref:`Solar Multiple <mslf-solarmultiple>`   for details.

**Solar Multiple**
  The field aperture area expressed as a multiple of the aperture area required to operate the power cycle at its design capacity. See :ref:`Solar Multiple <mslf-solarmultiple>`   for details.

**Field Aperture (m²)**
  The total solar energy collection area of the solar field in square meters. Note that this is less than the total mirror surface area.

.. note:: For simulations, SAM uses the calculated solar multiple and field aperture values shown in blue to the right of the inputs. The calculated value is likely to be different from the value you see on the System Design page because the field aperture changes in discrete steps based on the number of loops.

**Design point DNI (W/m²)**
  The design point direct normal radiation value, used in solar multiple mode to calculate the aperture area required to drive the power cycle at its design capacity. Also used to calculate the design mass flow rate of the heat transfer fluid for header pipe sizing.

**Actual field thermal power, MWt**
  The field thermal power output at design.

*Field Thermal Power (MWt) = Solar Multiple × Cycle Thermal Power (MWt)*

**Field thermal power w/ no defocus constraints**
  XX

**Loop inlet HTF temperature (ºC)**
  The temperature of HTF at the loop inlet under design conditions. The actual temperature during operation may differ from this value. SAM sets the power cycle's design outlet temperature equal to this value.

**Loop outlet HTF temperature (ºC)**
  The temperature of the HTF at the outlet of the loop under design conditions. During operation, the actual value may differ from this set point. This value represents the target temperature for control of the HTF flow through the solar field and will be maintained when possible.

**Number of loops**
  The number of loops in the field, equal to the solar multiple times the required number of loops at a solar multiple of 1.0. The required number of loops is rounded to the nearest integer to represent a realistic field layout.

Power Cycle
-----------

The power cycle design parameters determine the capacity of the power cycle, and the nameplate capacity of the system. See the :doc:`Power Cycle <mslf_power_cycle>` page for more detailed power cycle options and detailed parameters.

**Design turbine gross output (MWe)**
  The power cycle's design output, not accounting for parasitic losses. SAM uses this value to size system components, such as the solar field area when you use the solar multiple to specify the solar field size. 

**Estimated gross to net conversion factor**
  An estimate of the ratio of the electric energy delivered to the grid to the power cycle's gross output. SAM uses the factor to calculate the power cycle's nameplate capacity for capacity-related calculations, including the estimated total cost per net capacity value on the :doc:`Installation costs <../installation-costs/cc_linear_fresnel>`   and :doc:`Operating costs <../operating-costs/oc_operating>`   pages capacity-based incentives on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page, and the capacity factor reported in the :doc:`results <../getting-started/results_page>`  .

**Estimated net output at design (nameplate) (MWe)**
  The power cycle's nameplate capacity, calculated as the product of the design gross output and estimated gross to net conversion factor.

*Estimated Net Output at Design (MWe) = Design Gross Output (MWe) × Estimated Gross to Net Conversion Factor*

**Cycle thermal efficiency**
  The thermal to electric conversion efficiency of the power cycle under design conditions

**Cycle thermal power, MWt**
  The power cycle thermal power input at design.

*Cycle Thermal Power, MWt = Design Turbine Gross Output (MWe) ÷ Cycle Thermal Efficiency*

Thermal Storage
---------------

The thermal storage design parameters determine the size of the thermal energy storage system (TES). See the :doc:`Thermal Storage <mslf_thermal_storage>` page for detailed TES parameters.

**Hours of storage at design point (hours)**
  The thermal storage capacity expressed in number of hours of thermal energy delivered at the design power cycle thermal power. The physical capacity is the number of hours of storage multiplied by the power cycle design thermal input. Used to calculate the thermal energy system's maximum storage capacity.

.. _mslf-solarmultiple:

Solar Multiple
~~~~~~~~~~~~~~

.. include:: ../includes/snip_solar_multiple.rst

