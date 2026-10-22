
The land area is the amount of land required by the project for land purchase and/or land lease costs. SAM only uses the land area when you specify a land purchase cost on the Installation Costs page, or a land lease cost on the Operating Costs page.

.. note:: SAM's internal land cost calculations use values in $/acre. SAM displays values converted to hectares (ha) for reference, where 1 ha = 2.471 acre.

SAM can automatically calculate the land area based on the system design parameters, or you can enter a land area in acres/MWac of system capacity.

**Automatically calculate from module area**
  Choose this option to have SAM calculate a land area estimate from the total module area and ground coverage ratio (GCR).

**Enter area per capacity in acres/MWac**
  Choose this option to have SAM calculate the land area estimate from the system capacity and an acre per AC megawatt of system capacity value.

.. note:: The acres/MWac option applies to the AC capacity of the system, or the total AC inverter capacity, not the nameplate DC capacity of the array.

**Total module area, m****\ :sup:`2`\**
  This is an estimate of the area in the plane-of-array of all modules in the array. The total module area is not affected by the tilt angle or ground coverage ratio.

  For the Detailed PV model, the total module area is the product of the area of a single module and the number of modules in the system. The area of a single module is defined on the :doc:`Module <../detailed-photovoltaic-model/pv_module>`   page.

  For PVWatts, the the total module area is the nameplate DC capacity of the array divided by the module efficiency. The module efficiency is determined by the module type as described in the description of **Module type**.

**AC capacity, MWac**
  The system AC capacity, or total inverter capacity in AC megawatts.

  For the Detailed PV model, the AC capacity is the product of the inverter's maximum AC power from the Inveter page and the number of inverters from the System Design page.

  For PVWatts, the AC capacity is the system nameplate capacity divided by the DC to AC ratio.

**Land area per system capacity**
  When you choose **Enter area per capacity in acres/MWac**, type the land use estimate in acres per AC megawatt of system capacity. Typical values range between 5 and 10 acres/MWac.

**Land area multiplier**
  The land area multiplier applies to the total array area projected onto the ground as a way to account for additional land required for inverter pads, wiring, setbacks, etc. within the array. The array area projected onto the ground includes space between rows of modules and an empty row in front of and behind the array determined by the ground coverage ratio.

**Ground area occupied by array, acres or ha**
  An estimate of the land area occupied by the array, including space between rows determined by the ground coverage ratio (GCR) and any additional land specified by the land area multiplier.

  *Ground Area (acres) = Total Module Area (acres) / GCR × Land Area Multiplier*

  This estimate assumes that modules are arranged in rectangular rows, that all rows have the same dimensions, and that spacing between rows is uniform.

**Additional land area, acres or ha**
  Land area required in addition to the ground area occupied by the array. Choose **acres** or **ha** to change the units.

**Total estimated land area, acres or ha**
  The total land area for land purchase or land lease calculations including the total array projected onto the ground and any addition land area.

  *Total Estimated Land Area (acres) = Ground Area (acres) + Additional Land Area (acres)*