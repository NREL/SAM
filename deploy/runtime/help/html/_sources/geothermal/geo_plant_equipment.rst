Plant and Equipment
===================

.. note:: This topic is still under development. 


.. note:: If you have questions about SAM's geothermal model, please contact `sam.support@nlr.gov <mailto:sam.support@nlr.gov>`__.

Plant Configuration
~~~~~~~~~~~~~~~~~~~

The plant configuration describes the plant's conversion technology and how SAM models it.

**Specify plant output**
  The Specify Plant Output option allows you to specify the plant's electrical capacity in kilowatts. SAM calculates the plant size required to ensure that the plant's net output meets this output requirement, with enough extra power to supply parasitic load defined by the Calculated Design values on the :doc:`Resource page <geo_geothermal_resource>`  .

**Use exact number of wells**
  When you choose Use Exact Number of Wells, you specify the number of wells, and SAM calculates the plant's gross capacity based on the energy available from the wells, and the plant net output by subtracting the parasitic load from the gross output. The parasitic load is defined by the Calculated Design values on the :doc:`Resource page <geo_geothermal_resource>`  .

**Conversion Plant Type**
  The Conversion Plant Type determines the plant's steam-to-electricity conversion efficiency, also called "brine effectiveness.” The plant efficiency is different from the system efficiency, which also accounts for pumping losses from the parasitic load.

**Binary**
  When you choose the Binary option, you can specify the plant efficiency.

**Plant Efficiency**
  The steam-to-electricity conversion efficiency, expressed as a percentage of the theoretical maximum conversion efficiency.

**Flash**
  The Flash option allows you to choose from four subtypes that determine the plant efficiency. 

**Temperature Loss in Prod. Well**
  Constant temperature decrease of the fluid temperature as it exits the production well. Check this option to use the `Ramey wellbore model <https://www.opengeosys.org/stable/docs/benchmarks/heat-transport-bhe/pipe_flow_ebhe/>`__ to automatically calculate the temperature loss at each time step of the simulation.

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst

For the geothermal power model, the system availability losses are generation outages and losses that decrease the utilization factor, or net capacity factor, of the plant. It accounts for plant availability and ambient temperature variability but does not include the effect of declining resource productivity or degradation. 

The default system availability losses of 5% (or 95% utilization factor) assumes a ~3% (10 days per year) loss due to outages (7 days planned and 3 days unplanned outages), ~1% generation loss for plant maintenance (the entire plant is not shut down for routine maintenance but operates in flexible mode) and a 1% loss for ambient temperature variability effects. 

Temperature Decline
~~~~~~~~~~~~~~~~~~~

The temperature decline parameters determine when and how often the project will require that new wells be drilled, and are related to the total resource potential specified on the :doc:`Resource page <geo_geothermal_resource>`. 

For a description of the temperature decline inputs, see page 90 of the `GETEM User's Manual <https://www1.eere.energy.gov/geothermal/pdfs/getem_vol_i_technical_manual.pdf>`__ (2016)

Flash Technology
~~~~~~~~~~~~~~~~

The two flash technology inputs impact the plant conversion efficiency for the flash conversion type.

Pumping Parameters
~~~~~~~~~~~~~~~~~~

The **Production Well Flow Rate** and resource temperature specified on the :doc:`Resource page <geo_geothermal_resource>` dictate how much energy is available to the plant for conversion into electricity. The higher the flow rate, the more steam (or hot water) moves through the system, making thermal energy available for conversion, which, in turn, means fewer wells have to be drilled and therefore a lower capital expense. Production pump sizing parameters only apply for binary plant types, as flash plants assume no production pumping.

The remaining inputs impact the parasitic load for pumping. The **Injection Well Diameter** applies only when the resource type on the :doc:`Resource page <geo_geothermal_resource>` is EGS.