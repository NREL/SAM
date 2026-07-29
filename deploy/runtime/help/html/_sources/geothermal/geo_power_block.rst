Power Block
===========

The Power Block page allows you to specify the parameters of a power block that converts thermal energy from the geothermal resource to electric energy using a conventional steam Rankine cycle power plant.

The power cycle can use either an evaporative cooling system for wet cooling, an air-cooled system for dry cooling, or a hybrid cooling system with both wet and dry cooling.

The geothermal model runs a simulation over the multi-year life of the plant (defined by **Analysis Period** on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page) to account for the decline in geothermal resource. SAM models the geothermal resource decline on a monthly basis. 

The two monthly power block options result in a set of twelve calculations for each year in the plant life. The hourly option results in a set of 8,760 calculations for each year. For a project with a 30-year analysis period, the monthly power block option would result in 360 sets of calculations (12 months/year × 30 years = 360 months), and the hourly power block option would result in 262,800 sets of calculations (8,760 hours/year × 30 years = 262,800 hours).

Because it is unlikely that you will have weather data available for each of the years in the analysis period, SAM uses the same weather file for each year. The only value that might change from year to year in the performance model is the resource temperature as the geothermal resource degrades over time. For the hourly simulation option, SAM only calculates the monthly geothermal temperature decline. This is done for two reasons: 1) it helps maintain comparability between the monthly and hourly options; and 2) the resource temperature does not typically change measurably on an hourly basis, but might change from month to month.

Power Block Model
~~~~~~~~~~~~~~~~~

**Model**
  You can choose from two different power block model options: 

*   The GETEM option calculates the power block's monthly output.

*   The Power Block Monthly and Power Block Hourly options calculate either monthly or hourly power block output values, and use a more sophisticated algorithm based on physical principles using the power block model developed for SAM's physical parabolic trough model. For a detailed description, see Chapter 4 of Wagner M, 2008. Simulation and Predictive Performance Modeling of Utility-Scale Central Receiver System Power Plants. Master of Science Thesis. University of Wisconsin-Madison. (`PDF 13.6 MB <http://digital.library.wisc.edu/1793/45001>`__.

Power Block Design Point
~~~~~~~~~~~~~~~~~~~~~~~~

**Rated cycle conversion efficiency**
  The thermal to electric conversion efficiency of the power cycle under design conditions.

**Design inlet temperature (ºC)**
  The heat transfer fluid temperature at the power cycle inlet under design conditions. SAM sets this value to the plant design temperature on the :doc:`Plant and Equipment <geo_plant_equipment>`   page.

**Design outlet temperature (ºC)**
  The heat transfer fluid temperature at the power cycle outlet under design conditions.

**Boiler operating pressure (bar)**
  The steam pressure in the main Rankine cycle boiler at design, used to calculate the steam saturation temperature in the boiler, and thus the driving heat transfer temperature difference between the inlet heat transfer fluid and the steam in the boiler. 

**Steam cycle blowdown fraction**
  The fraction of the steam mass flow rate in the power cycle that is extracted and replaced by fresh water. This fraction is multiplied by the steam mass flow rate in the power cycle for each hour of plant operation to determine the total required quantity of power cycle makeup water. The blowdown fraction accounts for water use related directly to replacement of the steam working fluid. The default value of 0.013 for the wet-cooled case represents makeup due to blowdown quench and steam cycle makeup during operation and startup. A value of 0.016 is appropriate for dry-cooled systems to account for additional wet-surface air cooling for critical Rankine cycle components.

Cooling System
~~~~~~~~~~~~~~

**Condenser type**
Choose either an air-cooled condenser (dry cooling), evaporative cooling (wet cooling), or hybrid cooling system.

In hybrid cooling a wet-cooling system and dry-cooling share the heat rejection load. Although there are many possible theoretical configurations of hybrid cooling systems, SAM only allows a parallel cooling option. 

**Hybrid Dispatch**
  For hybrid cooling, the hybrid dispatch table specifies how much of the cooling load should be handled by the wet-cooling system for each of 6 periods in the year. The periods are specified in the matrices. Each value in the table is a fraction of the design cooling load. For example, if you want 60% of heat rejection load to go to wet cooling in Period 1, type 0.6 for Period 1, and then user your mouse to select the hours and months in the schedule that Period 1 applies, and type the number 1. See the :doc:`Weekday Weekend Schedule <../reference/weekday_schedule>`   reference topic for step-by-step instructions for using assigning periods to a schedule matrix. 

Directing part of the heat rejection load to the wet cooling system reduces the total condenser temperature and improves performance, but increases the water requirement. SAM sizes the wet-cooling system to match the maximum fraction that you specify in the hybrid dispatch table, and sizes the air-cooling system to meet the full cooling load.

 The hybrid dispatch option only works with the hourly power block model. The option does not work with the monthly or GETEM power block model.

**Ambient temperature at design (ºC)**
  The ambient temperature at which the power cycle operates at its design-point-rated cycle conversion efficiency. For the air-cooled condenser option, use a dry bulb ambient temperature value. For the evaporative condenser, use the wet bulb temperature.

**Ref. Condenser Water dT (ºC)**
  For the evaporative type only. The temperature rise of the cooling water across the condenser under design conditions, used to calculate the cooling water mass flow rate at design, and the steam condensing temperature.

**Approach temperature (ºC)**
  For the evaporative type only. The temperature difference between the circulating water at the condenser inlet and the wet bulb ambient temperature, used with the ref. condenser water dT value to determine the condenser saturation temperature and thus the turbine back pressure. 

**ITD at design point (ºC)**
  For the air-cooled type only. Initial temperature difference (ITD), difference between the temperature of steam at the turbine outlet (condenser inlet) and the ambient dry-bulb temperature.

**Condenser pressure ratio**
  For the air-cooled type only. The pressure-drop ratio across the air-cooled condenser heat exchanger, used to calculate the pressure drop across the condenser and the corresponding parasitic power required to maintain the air flow rate.

**Min condenser pressure**
  The minimum condenser pressure in inches if mercury prevents the condenser pressure from dropping below the level you specify. In a physical system, allowing the pressure to drop below a certain point can result in physical damage to the system. For evaporative (wet cooling), the default value is 1.25 inches of mercury. For air-cooled (dry cooling), the default is 2 inches of mercury. For hybrid systems, you can use the dry-cooling value of 2 inches of mercury.

**Cooling system part load levels**
  The cooling system part load levels tells the heat rejection system model how many discrete operating points there are. A value of 2 means that the system can run at either 100% or 50% rejection. A value of three means rejection operating points of 100% 66% 33%. The part load levels determine how the heat rejection operates under part load conditions when the heat load is less than full load. The default value is 2, and recommended range is between 2 and 10. The value must be an integer.