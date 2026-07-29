
You can model a system with a single device or more than one device.

System Sizing
-------------

Use the system sizing options to determine how to size the system.

**Use a single device**
  Model the system as a single device.

**Specify desired rated capacity**
  Automatically determine the number of devices based on a desired system capacity in kW. SAM attempts to find a number of devices that meets the desired array capacity you enter. The capacity of a single device is determined by the parameters on the :doc:`Wave Energy Converter <../marine-energy-wave/me_wave_converter>`   or :doc:`Tidal Energy Converter <../marine-energy-tidal/me_tidal_converter>`   page.

**Specify number of devices**
  Use this option to define an array of devices by specifying the number of rows and devices per row under **Array Layout**.

Array Layout
------------

The Array Layout parameters define the relative positions of devices when there is more than one device, and the lengths of electrical cable connecting the array to the shore. SAM can only model devices arranged in rectangular arrays.

.. note:: The array layout parameters are used to calculate cable lengths for cost calculations on the Installation Costs page. These parameters do not affect estimates of electrical losses on the Losses page.

**Number of rows**
  Number of device rows perpendicular to the wave direction.

**Devices per row**
  For arrays of more than one device, the number of devices in each row.

**Spacing between devices in a row, m**
  Distance between devices in each row in meters. This variable is hidden when there is a single device in the system.

**Row spacing**
  The distance between rows in meters. This variable is hidden when there is a single device in the system.

**Distance to shore, m**
  The distance between the edge of the array closest to the shore and the shoreline in meters. Used to calculate the export cable cost on the Installation Costs page.

**Water depth, m**
  The average depth of water in the array in meters.

**Use values from wave resource file**
  This input is only visible when the Time Series option is selected on the :doc:`Wave Resource <../marine-energy-wave/me_wave_resource>`   page. When this box is checked, the distance to shore and water depth inputs are filled with values taken from the weather data selected on the Wave Resource page. 

**Cable system overbuild, %**
  Overbuild margin for export and array cable.

**Floating array**
  Determines whether riser cable is included in the system. Riser cable is only required for floating arrays.

**Build export cable redundancy**
  Adds extra length to export cable length, assuming two export cables instead of one.

Calculated Cable Lengths
------------------------

The calculated cable length values are determined from the array layout parameters and used to calculate cable costs on the Installation Costs page. All lengths are in meters.

**Inter-array cable length, m**
  The total length of cable used within the array of devices.

*Cable Length = ( Devices per Row - 1 ) × Spacing Between Devices in a Row × Number of Rows + Row Spacing × (Number of Rows - 1)*

  This length is adjusted to meet the **Cable system overbuild** requirement:

*Adjusted Cable Length = Cable Length*   × ( 1 + Cable System Overbuild / 100% )

**Export cable length, m**
  The length of cable between the array and onshore grid connection point.

*Cable Length = ( Water Depth + Distance to Shore ) × ( 1 + Cable System Overbuild / 100% )*

  If you choose the **Build export redundancy** option:

*Cable Length with Redundancy = ( Water Depth + 2 × Distance to Shore ) × ( 1 + Cable System Overbuild / 100% )*

**Riser cable length, m**
  The length of cable from the seabed to the water surface that connects the floating device to the seabed cabling. Applies only when you choose the **Floating array** option.

*Riser Cable Length = 1.5 × Water Depth × Number of Devices*   × ( 1 + Cable System Overbuild / 100% )