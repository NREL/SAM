Temperature derating is an inverter function that decreases inverter power to prevent heat-related damage to internal components when the ambient temperature increases above a pre-defined value. SAM uses the information from the inverter temperature derate curves with ambient temperature data from the weather file to adjust the inverter efficiency in each time step of the simulation. As ambient temperature increases, the inverter efficiency decreases based on the data in the table. You can see the effect of inverter temperature derating using the following results variables:

* **Inverter thermal derate DC power loss (kWh/yr)**

* **DC inverter thermal derate loss (%)**

* **Inverter thermal derate DC power loss (kW)**

SAM adjusts the inverter efficiency based on the ambient temperature in the weather file based on the Efficiency - Ambient Temperature curve under **Inverter Temperature Derate Curves**. The default curve decreases the inverter efficiency as the ambient temperature increases above 52.8 degrees Celsius at a rate of 0.021% per degree of temperature increases. You can edit the values in the efficiency table to change the shape of the curve.

Temperature derating data is not included in the inverter library, but may be available on the inverter manufacturer datasheet. The default generic temperature derating curve is for an inverter that starts derating when the ambient temperature reaches 50°C at 1300 Vdc and shuts the inverter down when the ambient temperature reaches 55°C. You can use the default data if you do not expect the inverter to be exposed to high temperatures.

The following example shows SAM derating data for the inverter described in the SMA technical document "SUNNY BOY / SUNNY TRIPOWER Temperature Derating" available at https://files.sma.de/downloads/Temp-Derating-TI-en-15.pdf.

.. image:: /images/ss-pv-inverter-temp-curves.png
   :align: center
   :alt: ss-pv-inverter-temp-curves.png

**Vdc (V)**
  The inverter operating DC voltage for each temperature derating curve. Assign one row for each voltage level. The table must include at least one **Vdc** value. Change the value of **Rows** to add or remove rows.

**Tstart (C)**
  The temperature at which inverter derating starts for the given **Vdc**. To add a temperature-slope pair, add two columns to the table. Change the value of **Cols** to add or remove columns.

**Slope (1/C)**
  The inverter efficiency rate of change beginning at the given **Tstart**.

**Rows**
  Determines the number of voltage levels. Must be at least 1.

**Cols**
  Determines the number of **Tstart**/**Slope** pairs. Acceptable values are 3, 5, 7 and 9 for up to four pairs.

**Update plot**
  After changing values in the table, click **Update plot** to change the graph.

.. note:: To avoid excessive temperature derating losses for large inverters, make sure the highest Vdc voltage value in the temperature derate curves is greater than or equal to the inverter's maximum VDC voltage rating.
  
   The temperature derating model is not part of the original Sandia Performance Model for Grid-Connected Photovoltaic Inverter. It is a special implementation for SAM's Detailed PV model.