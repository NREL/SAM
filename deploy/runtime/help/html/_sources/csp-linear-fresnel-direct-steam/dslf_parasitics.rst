Parasitics
==========

The parasitic loss variables are factors that SAM uses to calculate the estimated total parasitic loss and hourly parasitic losses, which are described in more detail below.

SAM calculates two types parasitic loss values. The first is an estimate of the total parasitic losses used to calculate the power cycle design thermal input, and the second are the hourly values calculated during simulation of the system's performance.

.. note:: Parasitic losses from components that do not exist in the system should be set to zero.

Parasitic Energy Consumption
----------------------------

**Piping thermal loss coefficient (W/K-m2-aper)**
  Thermal loss per area of collector aperture as calculated on the :doc:`Solar Field <dslf_solar_field>`   page. 

**Tracking Power  (W/m2)**
  The electric power in Watts per area of collector aperture required by the tracking mechanism of each collector in the field during hours of operation.

**Tracking Power Loss (W)**
  SAM calculates the power loss in Watts based on the W/m2 value you specify above.

**Fraction of rated gross power consumed at all times**
  The fraction of design-point gross power output from the power cycle that is used for parasitics associated with facility operation, HVAC, control, lighting, etc.

**Fixed parasitic loss (MWe)**
  SAM calculates the fixed loss based on the fraction you specify above.

**Balance of Plant Parasitic (MWe/MWcap)**
  Losses as a fraction of the power cycle electrical power output that apply in hours when the power block operates.

**Aux heater, boiler parasitic (MWe/MWcap)**
  Parasitic power consumption incurred during operation of the backup fossil boiler, as a function of thermal power production of the fossil system. This parasitic is only applicable for systems with active fossil backup, and applies during hours in which the fossil system produces thermal power.