
If the heat transfer fluid you want to use in the solar field is not included in the Field HTF Fluid list, you can define a custom heat transfer fluid using the User-defined option in the list. To define a custom fluid, you need to know the following properties for at least two temperatures:

* Temperature, ºC

* Specific heat, kJ/kg-K

* Density, kg/m³

* Viscosity, Pa-s

* Kinematic viscosity, m²-s (not required, see note below)

* Conductivity, W/m-K

* Enthalpy, J/kg (not required, see note below)

.. note:: The kinematic viscosity and enthalpy data in the table are not used by the CSP models. These properties are redundant: Kinematic viscosity is the ratio of viscosity to density, and the heat balance equations use specific heat instead of enthalpy.

To define a custom heat transfer fluid:

#. In the Field HTF fluid list, click **User-defined**.

#. In the Edit Material Properties table, change **Number of data points** to 2 or higher. The number should equal the number of temperature values for which you have data.

#. Type values for each property in the table.

You can also import data from a text file of comma-separated values. Each row in the file should contain properties separated by commas, in the same the order that they appear in the Edit Material Properties window. Do not include a header row in the file.
 
.. note:: Each row in the materials property fluid table must be for a set of properties at a specific temperature. No two rows should have the same temperature value.

   SAM calculates property values from the table using linear interpolation.

   The rows in the table must sorted by the temperature value, in either ascending or descending order.

   The physical trough model uses the temperature, specific heat, density, viscosity, and conductivity values. It ignores the enthalpy and kinematic viscosity values (the :doc:`empirical trough model <../csp-empirical-trough-model/troughempirical_overview>` does use those values).

   For the physical trough model, if you specify user-defined HTF fluids with the same properties for the solar field and thermal storage system, on the :doc:`Thermal Storage <../csp-physical-trough-model/troughphysical_thermal_storage>` page, SAM disables the **Hot side HX approach temp** and **Cold side HX approach temp** inputs, and sets them to zero internally to represent a system with no heat exchanger. (When the hot and cold side approach temperatures are zero, **Thermal storage exergetic efficiency** is one.)