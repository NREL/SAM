System Design
=============

The System Design page shows inputs for design point parameters that determine the system's nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.

.. note:: All of the system design inputs are nominal values, or values at the system's design point. SAM calculates actual values during simulation and reports them in the :doc:`results <../getting-started/results_page>`.

Heater
~~~~~~

**Heater multiple**
  The heater multiple determines the heater's nominal thermal power as a multiple of the **Cycle thermal power**.

**Heater thermal power**
  The heater's thermal output under design conditions. SAM displays the heater thermal power on :doc:`Installation costs <../installation-costs/cc_etes>`   page.

*Heater Thermal Power (MWt) = Heater Multiple × Cycle Thermal Power (MWt)*

**HTF hot temperature**
  The HTF temperature at the heater outlet under design conditions.

**HTF cold temperature**
  The HTF temperature at the heater inlet under design conditions.

Thermal Storage
~~~~~~~~~~~~~~~

**Full load hours of storage, hours**
  The nominal thermal storage capacity expressed in hours at full load: The number of hours that the storage system can supply heat at the cycle thermal power.

**Heater hours of storage, hours**
  The nominal thermal storage capacity expressed in hours of heater thermal power.

*Heater Hours of Storage (h) = Full Load Hours of Storage (h) ÷ Heater Multiple*

Power Cycle
~~~~~~~~~~~

**Design turbine gross output, MWe**
  The power cycle's electrical output at the design point, not accounting for parasitic losses, such as air cooler fan power or HTF pumping power. This value together with the gross-to-net conversion factor and nominal cycle thermal efficiency determines the nominal cycle thermal power.

**Estimated gross-to-net conversion factor**
  An estimate of the ratio of the electric energy delivered to the grid to the power cycle gross output. The value represents the value of expected parasitic losses at the design point. SAM uses the factor to calculate the power cycle's nameplate capacity for capacity-related calculations, including the estimated total cost per net capacity value for cost calculations, and the capacity factor reported in the results.

**Estimated net output at design (nameplate), MWe**
  The power cycle nominal electrical output.

*Estimated Net Output at Design (MWe) = Design Gross Output (MWe) × Estimated Gross-to-net Conversion Factor*

**Cycle thermal efficiency**
  The thermal-to-electric gross conversion efficiency of the power cycle at the design point. Note that this a nominal value: SAM calculates the power cycle's actual efficiency during the simulation and reports the actual value each time step in the :doc:`results <../getting-started/results_page>`  . This value together with the turbine gross output determines the cycle nominal thermal power.

**Cycle thermal power, MWt**
  The thermal power required at the power cycle inlet for it to operate at its design point.

*Cycle Thermal Power (MWt) = Design Turbine Gross Output (MWe) ÷ Cycle Thermal Efficiency*