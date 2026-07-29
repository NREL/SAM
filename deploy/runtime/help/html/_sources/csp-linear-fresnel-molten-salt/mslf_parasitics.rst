Parasitics
==========

The variables on the Parasitics page define electrical loads in the system. For each hour of the simulation, SAM calculates the parasitic load and subtracts it from the power cycle's gross electrical output to calculate the net electrical output.

Parasitics
~~~~~~~~~~

**Non-receiver piping thermal loss coefficient, W/m****\ :sup:`2`\****-K**
  The thermal loss coefficient that is used to calculate thermal losses from piping between receivers, crossover piping, header piping, and runner piping. The coefficient specifies the number of thermal watts lost from the system as a function of the piping surface area, and the temperature difference between the fluid in the piping and the ambient air (dry bulb temperature). The length of crossover piping depends on the row spacing variable on the :doc:`Solar Field <../csp-physical-trough-model/troughphysical_solar_field>`   page, and the piping distance between assemblies on the :doc:`Collectors <../csp-physical-trough-model/troughphysical_collectors_scas>`   page.

**Tracking power in Watts per SCA drive**
  The amount of electrical power consumed by a single collector tracking mechanism. SAM only calculates tracking losses during hours when collectors are actively tracking the sun. The total field tracking power is calculated by multiplying this value by the number of loops in the field and number of assemblies per loop specified on the :doc:`Solar Field <../csp-physical-trough-model/troughphysical_solar_field>`   page.

**Pumping power requed to move 1 kg of HTF through PB loop, kJ/kg**
  A coefficient used to calculate the electric power required to pump heat transfer fluid through the power cycle. SAM applies the coefficient to all heat transfer fluid flowing through the power cycle. The coefficient can alternatively be defined as the pumping power divided by the mass flow rate kW/kg-s, which is equivalent to the units kJ/kg.

**Pumping power requed to move 1 kg of HTF through TES loop, kJ/kg**
  A coefficient used to calculate the electric power consumed by pumps to move heat transfer fluid through the storage heat exchanger on both the solar field side and the storage tank side (for cases where a heat exchanger exists, specified on the :doc:`Thermal Storage <../csp-physical-trough-model/troughphysical_thermal_storage>`   page). This coefficient is applied separately to the solar field flow and the tank flow.

**Fraction of rated gross power constantly consumed**
  A fixed electric load applied to all hours of the simulation, expressed as a fraction of rated gross power at design from the :doc:`Power Cycle <mslf_power_cycle>`   page.

**Balance of plant parasitic, MWe/MWcap**
  A parasitic load that is applied as a function of the thermal input to the power cycle.

**Aux heater, boiler parasitic, MWe/MWcap**
  A parasitic load that is applied as a function of the thermal output of the auxiliary fossil-fired heaters. Applies only when the system includes fossil backup. See :doc:`the fossil backup inputs on the Power Cycle page <mslf_power_cycle>`  .

Design Point Totals
-------------------

The design point total values represent the maximum limit of parasitic losses, and are an estimate of the losses. SAM calculates actual parasitic losses during the simulation and reports them in :doc:`time series <../results/timeseries>` results with variable names beginning with "Parasitic".

**Tracking parasitic loss**
  .. image:: ../images/EQ_TRP-TrackingPower.png
     :align: center
     :alt: EQ_TRP-TrackingPower.png

**Fixed parasitic loss**
  .. image:: ../images/EQ_TRP-FractionofRatedGross.png
     :align: center
     :alt: EQ_TRP-FractionofRatedGross.png

