Aux with and without Solar
==========================

The Solar Water Heating Model calculates the :doc:`annual energy savings <mtp_annual_energy>` by a solar water heating system designed to meet the hourly hot water heating load defined by the hot water draw on the :doc:`SWH System <../solar-water-heating/swh_system>` page. To do so, SAM calculates the following quantities, which you can see in the :doc:`Tables <../results/data>` on the Results page:

**Aux With Solar**
  The energy supplied by the auxiliary electric water heating system to supplement energy from the solar collectors.

**Aux Without Solar**
  The energy that would be required from a standard electric hot water system if the solar system were not present.

SAM assumes that the solar water heating system cannot export power. The annual energy saved by the solar water heating system as follows:

```p_pump = pump_rated_power / pump_efficiency```

```For each time step:```

```p_saved = p_without_solar - p_with_solar - p_pump```

```If p_saved > p_load:```

```p_saved = p_load```

```annual_energy_saved = sum( p_saved )```