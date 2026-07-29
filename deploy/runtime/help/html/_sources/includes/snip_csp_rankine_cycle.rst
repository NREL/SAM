
The power cycle page displays variables that specify the design operating conditions for the steam Rankine cycle used to convert thermal energy to electricity.

**Steam cycle blowdown fraction**
  The fraction of the steam mass flow rate in the power cycle that is extracted and replaced by fresh water. This fraction is multiplied by the steam mass flow rate in the power cycle for each hour of plant operation to determine the total required quantity of power cycle makeup water. The blowdown fraction accounts for water use related directly to replacement of the steam working fluid. The default value of 0.013 for the wet-cooled case represents makeup due to blowdown quench and steam cycle makeup during operation and startup. A value of 0.016 is appropriate for dry-cooled systems to account for additional wet-surface air cooling for critical Rankine cycle components.

**Turbine inlet pressure control**
  Determines the power cycle working fluid pressure during off-design loading.

**Fixed Pressure:** The power block maintains the design high pressure of the power cycle working fluid during off-design loading.

**Sliding Pressure:** The power block decreases the high pressure of the power cycle working fluid during off-design loading.

**Condenser type**
  Choose either an air-cooled condenser (dry cooling), evaporative cooling (wet cooling), or hybrid cooling system. Only evaporative and air-cooled are available for electric thermal energy storage systems.

  The air-cooled condenser model utilizes a second-order bi-variate polynomial in terms of normalized ambient temperature and normalized heat rejection to determine normalized condenser pressure as shown in the graph below. Ambient temperature (converted to Kelvin) and heat rejection are normalized by their design conditions, while condenser pressure is normalized by the minimum condenser pressure. This model is valid for normalized ambient temperatures greater than 0.9. For conditions lower than this threshold, the condenser pressure is set to its minimum value.

  .. image:: /images/IMG_Rankine-cycle-air-cooling.png
     :align: center
     :alt: IMG_Rankine-cycle-air-cooling.png

  In hybrid cooling, a wet-cooling system and dry-cooling share the heat rejection load. Although there are many possible theoretical configurations of hybrid cooling systems, SAM only allows a parallel cooling option.

  Specify the hybrid cooling dispatch fractions on the System Control page.

**Ambient temperature at design, ºC**
  The ambient temperature at which the power cycle operates at its design-point-rated cycle conversion efficiency. For the air-cooled condenser option, use a dry bulb ambient temperature value. For the evaporative condenser, use the wet bulb temperature.

**ITD at design point, ºC**
  For the air-cooled type only. Initial temperature difference (ITD), difference between the temperature of steam at the turbine outlet (condenser inlet) and the ambient dry-bulb temperature.

.. note:: When you adjust the ITD, you are telling the model the conditions under which the system will achieve the thermal efficiency that you've specified. If you increase the ITD, you should also modify the thermal efficiency (and/or the design ambient temperature) to accurately describe the design-point behavior of the system. The off-design penalty in the modified system will follow once the parameters are corrected.

**Reference condenser water dT, ºC**
  For the evaporative condenser type only. The temperature rise of the cooling water across the condenser under design conditions, used to calculate the cooling water mass flow rate at design, and the steam condensing temperature.

**Approach temperature, ºC**
  For the evaporative type only. The temperature difference between the circulating water at the condenser inlet and the wet bulb ambient temperature, used with the ref. condenser water dT value to determine the condenser saturation temperature and thus the turbine back pressure. 

**Condenser Pressure Ratio**
  For the air-cooled type only. The pressure-drop ratio across the air-cooled condenser heat exchanger, used to calculate the pressure drop across the condenser and the corresponding parasitic power required to maintain the air flow rate.

**Minimum condenser pressure**
  The minimum condenser pressure in inches of mercury prevents the condenser pressure from dropping below the level you specify. In a physical system, allowing the pressure to drop below a certain point can result in physical damage to the system. For evaporative (wet cooling), the default value is 1.25 inches of mercury. For air-cooled (dry cooling), the default is 2 inches of mercury. For hybrid systems, you can use the dry-cooling value of 2 inches of mercury.

**Cooling system part load levels**
  The cooling system part load levels tells the heat rejection system model how many discrete operating points there are. A value of 2 means that the system can run at either 100% or 50% rejection. A value of three means rejection operating points of 100% 66% 33%. The part load levels determine how the heat rejection operates under part load conditions when the heat load is less than full load. The default value is 2, and recommended range is between 2 and 10. The value must be an integer.