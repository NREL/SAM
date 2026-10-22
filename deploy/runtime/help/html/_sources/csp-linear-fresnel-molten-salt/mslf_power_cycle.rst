Power Cycle
===========

The power cycle converts thermal energy to electric energy. The power cycle is assumed to consist of a superheated two-stage turbine with multiple extractions for feedwater heating and a reheat extraction between the high and low pressure turbine stages. You specify the design-point efficiency of this cycle on the Power Cycle page, and SAM models the part-load behavior with normalized performance curves as a function of steam inlet temperature, mass flow rate, and ambient temperature. The ambient temperature correction uses the wet-bulb temperature for wet-cooled systems and hybrid systems and the dry-bulb temperature for dry cooled and hybrid cooled systems. 

The power cycle page displays variables that specify the design operating conditions for the steam Rankine cycle used to convert thermal energy to electricity.

System Design Point Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These values are from the :doc:`System Design <mslf_system_design>` page are for reference.

Plant Capacity
~~~~~~~~~~~~~~

**Reference output electric power at design condition, MWe**
  The power cycle's design output, not accounting for parasitic losses.

**Estimated gross to net conversion factor**
  An estimate of the ratio of the electric energy delivered to the grid to the power cycle's gross output. SAM uses the factor to calculate the power cycle's nameplate capacity for capacity-related calculations, including the estimated total cost per net capacity value on the Installation costs page, and the capacity factor reported in the results.

**Estimated net output at design (nameplate), MWe**
  The power cycle's nameplate capacity, calculated as the product of the design gross output and estimated gross to net conversion factor.

*Net Nameplate Capacity (MWe) = Reference Output (MWe) × Estimated Gross to Net Conversion Factor*

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst

Power Block Design Point
~~~~~~~~~~~~~~~~~~~~~~~~

**Rated cycle conversion efficiency**
  The thermal to electric conversion efficiency of the power cycle under design conditions.

**Reference HTF outlet temperature at design, °C**
  The design temperature in degrees Celsius of the cold heat transfer fluid at the power block outlet. The design values are the operating conditions at which the power block operates at its nameplate capacity.

**Reference HTF inlet temperature at design, °C**
  The design temperature in degrees Celsius of the hot heat transfer fluid at the power block inlet. Design HTF inlet temperature can be different from receiver outlet temperature when power block design specifications require a different inlet temperature for maximum efficiency. The design values are the operating conditions at which the power block operates at its nameplate capacity.

**Power block blowdown steam fraction**
  The fraction of the steam mass flow rate in the power cycle that is extracted and replaced by fresh water. This fraction is multiplied by the steam mass flow rate in the power cycle for each hour of plant operation to determine the total required quantity of power cycle makeup water. The blowdown fraction accounts for water use related directly to replacement of the steam working fluid. The default value of 0.013 for the wet-cooled case represents makeup due to blowdown quench and steam cycle makeup during operation and startup. A value of 0.016 is appropriate for dry-cooled systems to account for additional wet-surface air cooling for critical Rankine cycle components.

**Fossil backup boiler LHV efficiency**
  The auxiliary fossil backup boiler's lower heating value efficiency, used to calculate the quantity of natural gas required by the boiler.

  If your system includes an auxiliary boiler, choose a fossil dispatch mode and define the fossil fill fractions as described below.

**Aux heater outlet set temp, °C**
  The temperature set point for the auxiliary heaters for the fossil backup system.

**Fossil dispatch mode**

  The fossil dispatch mode determines how the optional auxiliary fossil (natural gas) backup boiler is configured and how it operates. Use the **fossil fill fraction** input under **Dispatch Control** on the :doc:`Thermal Storage <mslf_thermal_storage>`   page to determine when the auxiliary boiler operates. Use the fossil backup boiler LHV efficiency value under Plant Design to define the boiler's lower heating value efficiency.

  If your system has an auxiliary fossil backup boiler, and you want to account for fuel costs in the financial model, you should assign a fossil fuel cost under Operation and Maintenance costs on the :doc:`Installation costs <../installation-costs/cc_linear_fresnel>`   page.

.. note:: If the **Fossil fill fraction** under **Dispatch Control** on the Thermal Storage page is zero for all periods, the system does not include an auxiliary fossil backup boiler, and you can ignore the fossil dispatch mode input.

**Minimum backup level**
  In the Minimum Backup Level mode, the auxiliary boiler is in parallel with the power cycle and supplies heat to the HTF at the power cycle inlet when the solar field thermal output falls below the level defined by the fossil fill fractions and weekday/weekend schedules.

  For example, for an hour with a fossil fill fraction of 1.0 when solar energy delivered to the power cycle is less than that the design thermal input power, the auxiliary boiler would supply enough energy to "fill" the missing heat so that the thermal power at the power cycle inlet is at the design point. If, in that scenario, solar energy from the field is driving the power cycle at full load, the auxiliary boiler would not operate. For a fossil fill fraction of 0.75, the auxiliary boiler would only be fired when solar output drops below 75% of the power cycle's design thermal input power.

**Supplemental operation**
  In the Supplemental Operation mode, the auxiliary boiler is in parallel with the power cycle and supplies sufficient heat to the HTF at the power cycle inlet for the power cycle to operate at its design point, where the design point is determined by the fossil fill fractions and weekday/weekend schedules.

  Operation of the power cycle in a given hour is constrained by the **Max turbine over design operation fraction** and **Minimum operation fraction**. For hours that the energy added by the auxiliary backup boiler is insufficient to meet the minimum requirement, the backup boiler is not dispatched.

Plant Control
~~~~~~~~~~~~~

**Low resource standby period, hours**
  During periods of insufficient flow from the heat source due to low thermal resource, the power block may enter standby mode. In standby mode, the cycle can restart quickly without the startup period required by a cold start. The standby period is the maximum number of hours allowed for standby mode. This option is only available for systems with thermal storage. Default is 2 hours.

**Fraction of thermal power needed for standby**
  The fraction of the turbine's design thermal input required from storage to keep the power cycle in standby mode. This thermal energy is not converted into electric power. Default is 0.2.

**Time needed for power block startup, hours**
  The time in hours that the system consumes energy at the startup fraction before it begins producing electricity. If the startup fraction is zero, the system will operate at the design capacity over the startup time. Default is 0.5 hours.

**Fraction of thermal power needed for startup**
  The fraction of the turbine's design thermal input required by the system during startup. This thermal energy is not converted to electric power. Default is 0.75.

**Startup temperature**
  The minimum HTF temperature at the power cycle inlet required for the power cycle to start after a period of not running.

**Maximum turbine over design operation fraction**
  The maximum allowable power block output as a fraction of the electric nameplate capacity. Whenever storage is not available and the solar resource exceeds the design value of 950 W/m2, some heliostats in the solar field are defocused to limit the power block output to the maximum load. Default is 1.05.

**Minimum turbine operation fraction before shutdown**
  The fraction of the nameplate electric capacity below which the power block does not generate electricity. Whenever the power block output is below the minimum load and thermal energy is available from the solar field, the field is defocused. For systems with storage, solar field energy is delivered to storage until storage is full. Default is 0.25.

**Turbine inlet pressure control**
  Determines the power cycle working fluid pressure during off-design loading.

**Fixed Pressure:** The power block maintains the design high pressure of the power cycle working fluid during off-design loading.

**Sliding Pressure:** The power block decreases the high pressure of the power cycle working fluid during off-design loading.

Cooling System
~~~~~~~~~~~~~~

**Condenser type**
  The type of cooling system: Air-cooled condenser (dry cooling), evaporative cooling (wet cooling), or hybrid cooling system.

  In hybrid cooling a wet-cooling system and dry-cooling share the heat rejection load. Although there are many possible theoretical configurations of hybrid cooling systems, SAM only allows a parallel cooling option. 

**Reference ambient temperature at design point, ºC**
  The ambient temperature at which the power cycle operates at its design-point-rated cycle conversion efficiency. For the air-cooled condenser option, use a dry bulb ambient temperature value. For the evaporative condenser, use the wet bulb temperature.

**Reference condenser cooling water inlet/outlet T diff, ºC**
  For the evaporative type only. The temperature rise of the cooling water across the condenser under design conditions, used to calculate the cooling water mass flow rate at design, and the steam condensing temperature.

**Cooling tower approach temperature, ºC**
  For the evaporative type only. The temperature difference between the circulating water at the condenser inlet and the wet bulb ambient temperature, used with the ref. condenser water dT value to determine the condenser saturation temperature and thus the turbine back pressure. 

**ITD at design point for dry system(ºC)**
  For the air-cooled type only. Initial temperature difference (ITD), difference between the temperature of steam at the turbine outlet (condenser inlet) and the ambient dry-bulb temperature.

.. note:: When you adjust the ITD, you are telling the model the conditions under which the system will achieve the thermal efficiency that you've specified. If you increase the ITD, you should also modify the thermal efficiency (and/or the design ambient temperature) to accurately describe the design-point behavior of the system. The off-design penalty in the modified system will follow once the parameters are corrected.

**Condenser pressure ratio**
  For the air-cooled type only. The pressure-drop ratio across the air-cooled condenser heat exchanger, used to calculate the pressure drop across the condenser and the corresponding parasitic power required to maintain the air flow rate.

**Minimum condenser pressure**
  The minimum condenser pressure in inches if mercury prevents the condenser pressure from dropping below the level you specify. In a physical system, allowing the pressure to drop below a certain point can result in physical damage to the system. For evaporative (wet cooling), the default value is 1.25 inches of mercury. For air-cooled (dry cooling), the default is 2 inches of mercury. For hybrid systems, you can use the dry-cooling value of 2 inches of mercury.

**Number of part-load increments for heat rejection**
  The cooling system part load levels tells the heat rejection system model how many discrete operating points there are. A value of 2 means that the system can run at either 100% or 50% rejection. A value of three means rejection operating points of 100% 66% 33%. The part load levels determine how the heat rejection operates under part load conditions when the heat load is less than full load. The default value is 2, and recommended range is between 2 and 10. The value must be an integer.