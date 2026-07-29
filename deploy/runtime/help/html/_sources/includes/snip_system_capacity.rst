
The system nameplate capacity depends on the technology being modeled. SAM converts the capacity value to the appropriate units (MW, kW, or W) before using it to calculate metrics such as the capacity factor, or the value of costs and incentives expressed in $/W. For the photovoltaic models, the capacity is a DC power rating, and for the others it is an AC power rating. For the Solar Water Heating Model, the capacity is a thermal rating, while for the others it is an electric rating.

.. note:: For the Detailed Photovoltaic and PVWatts models, SAM calculates both a DC capacity factor and an AC capacity factor. The table below shows the value used for both the AC and DC system capacity for these calculations.

Table . Nameplate system capacity values for each technology.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Performance Model
     - System Capacity
     - Input Page
   * - Detailed Photovoltaic (DC)
     - Nameplate DC capacity (kWdc)
     - :doc:`System Size <../detailed-photovoltaic-model/pv_system_size>`
   * - Detailed Photovoltaic (AC)
     - Total AC capacity (kWac) (for AC capacity factor)
     - :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>`
   * - PVWatts (DC)
     - DC rating (kWdc)
     - :doc:`System Design <../pvwatts/pvwatts_system_design>`
   * - PVWatts (AC)
     - Rated inverter size (kWac) (for AC capacity factor)
     - :doc:`System Design <../pvwatts/pvwatts_system_design>`
   * - High Concentration Photovoltaic  (DC)
     - System nameplate capacity
     - :doc:`Array <../high-concentration-photovoltaic/hcpv_array>`
   * - Detailed PV-Battery (DC)
     - Nameplate DC capacity (kWdc) (does not include battery nominal bank power)
     - :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>`
   * - PVWatts-Battery (DC)
     - System nameplate capacity (kWdc) (does not include battery power)
     - :doc:`System Design <../pvwatts/pvwatts_system_design>`
   * - Custom Generation Profile - Battery (AC)
     - Nameplate capacity (kWac) (does not include battery nominal bank power)
     - :doc:`Power Plant <../custom-generation/custom_generation_profile>`
   * - CSP Parabolic Trough - Physical (AC)
     - Estimated net output at design (Nameplate) (MWe)
     - :doc:`Power Cycle <../csp-physical-trough-model/troughphysical_power_cycle>`
   * - CSP Parabolic Trough - Empirical (AC)
     - Estimated net output at design (MWe)
     - :doc:`Power Block <../csp-empirical-trough-model/troughempirical_power_block>`
   * - CSP Molten Salt Power Tower (AC)
     - Estimated net output at design (Nameplate) (MWe)
     - :doc:`Power Cycle <../csp-power-tower-molten-salt/mspt_power_cycle>`
   * - CSP Generic Solar (AC)
     - Estimated net output at design (Nameplate) (MWe)
     - :doc:`Power Block <../csp-generic-solar/gss_power_block>`
   * - CSP Linear Fresnel (AC)
     - Net nameplate capacity (MWe)
     - :doc:`Power Cycle <../csp-linear-fresnel-direct-steam/dslf_power_cycle>`
   * - Custom Generation Profile (AC)
     - Nameplate capacity (kWe)
     - :doc:`Power Plant <../custom-generation/custom_generation_profile>`
   * - Geothermal
     - Net plant output (MWe)
     - :doc:`Plant and Equipment <../geothermal/geo_plant_equipment>`
   * - Water Heating Model (Thermal)
     - Nameplate capacity (kWt)
     - :doc:`SWH System <../solar-water-heating/swh_system>`
   * - Wind Power (AC)
     - System nameplate capacity (kWe)
     - :doc:`Wind Farm <../wind-power/wind_farm>`
   * - Biopower (AC)
     - Nameplate capacity, Gross (kWe)
     - :doc:`Plant Specs <../biopower/biopower_plant_specs>`