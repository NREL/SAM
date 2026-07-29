Hybrid System
=============

The Hybrid System page shows values AC capacity values from other input pages to give you a summary of the hybrid system's design.

To change any of the capacity values shown on the Hybrid System page, go to the subsystem page shown in the descriptions below.

Generation Capacity
~~~~~~~~~~~~~~~~~~~

The AC generation capacity of each subsystem in the hybrid system.

**PV Capacity, kWac**
  The nameplate AC capacity of the PV system.

  For hybrid systems with the PVWatts model, **Rated inverter size** from the :doc:`PVWatts System Design <../pvwatts/pvwatts_system_design>`   page.

  For the Detailed PV model, **Total AC capacity** from the :doc:`Photovoltaic System Design <../detailed-photovoltaic-model/pv_system_size>`   page.

.. note:: For hybrid systems, the total generation capacity is based on the photovoltaic subsystem's total inverter capacity in AC kilowatts. However, the capacity-based incentive (CBI) and capacity-based operating cost, if defined, are calculated from the total array capacity in DC kilowatts.

**Wind system capacity, kWac**
  Nameplate AC capacity of wind farm, **System nameplate capacity** from :doc:`Wind Farm <../wind-power/wind_farm>`   page.

**Fuel cell capacity, kWac**
  Fuel cell stack rated capacity, **Total cell stack nameplate** from the :doc:`Fuel Cell <../fuel-cell/fuelcell_fuel_cell>`   page.

**Custom generation capacity, kWac**
  Custom generation profile system nameplate capacity, **Nameplate Capacity** from the :doc:`Generation Profile <../custom-generation/custom_generation_profile>`   page.

Total Generation Capacity
~~~~~~~~~~~~~~~~~~~~~~~~~

The total nameplate capacity of the hybrid system, calculated as the sum of nameplate capacities of subsystems included in the hybrid system.

**Hybrid system capacity, kWac**
  The nameplate capacity of the hybrid system. Used for operation and maintenance costs specified in $/kW-year on the :doc:`Hybrid System costs <hybrid_system_costs>`   page, and to calculate capacity-based incentives on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.

Storage Capacity
~~~~~~~~~~~~~~~~

The AC power capacity of the battery storage system.

**Battery maximum discharge rate, kWac**
  The battery's maximum AC discharge rate, Maximum discharge power (AC) from the :doc:`Battery Cell and System <../battery-storage/battery_storage>`   page.