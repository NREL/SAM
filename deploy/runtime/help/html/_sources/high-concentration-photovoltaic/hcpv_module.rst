Module
======

The HCPV Module page inputs are for the high concentration photovoltaic performance model.

High Concentration Photovoltaic (HCPV) Module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Single cell area, cm²**
  The area in square centimeters of one cell in the module.

**Number of cells**
  The number of cells in a single module.

**Concentration ratio**
  The ratio of lens area to cell area. SAM uses this value to calculate the Overall Module Area based on the Single Cell Area and Number of Cells that you specify.

**Optical error factor**
  SAM applies this factor to the plane-of-array beam irradiance to adjust the value to account for losses due to lens optical error.

**Alignment loss factor**
  SAM applies this factor to the plane-of-array beam irradiance to adjust the value to account for losses due to alignment error.

**Wind flutter loss factor (per m/s)**
  SAM uses this factor to reduce the cell power value based on the wind speed to account for losses due to motion of the module caused by the wind. For each time step in the simulation, SAM reduces the calculated cell output power by *1 - Flutter Loss Factor × Wind Speed*  .

**Maximum Power (Pmp), Wdc**
  The module's maximum power point rating in DC Watts.

*Maximum Power (Wdc) = Reference Cell Efficiency (%) ÷ 100*
 *× Reference POA Irradiance (W/m²)*
 *× Overall Module Area (m²)*
 *× Optical Error Factor*
 *× (1 - Wind Flutter Loss Factor × 4 m/s)*
 *× Alignment Loss Factor*
 *× Modifier at AM 1.5*

**Overall module area, m²**
  The module's reflective area in square meters.

*Overall Module Area (m²) = Concentration Ratio × Single Cell Area (cm²) × 0.0001 (m²/cm²) × Number of Cells*

**Estimated reference module efficiency, %**
  The module's nominal efficiency.

*Estimated Module Efficiency (%) = Reference Cell Efficiency (%)*
 *× Optical Error Factor*
 *× (1 - Wind Flutter Loss Factor × 4 m/s)*
 *× Alignment Loss Factor*
 *× Modifier at AM 1.5*

Spectral Effects
~~~~~~~~~~~~~~~~

As you enter air mass modifier coefficients, SAM displays the air mass modifier values as a function of solar zenith angle in the graph that SAM will use during the simulation.

**Air mass modifier coefficients (a0 - a4)**
  Air mass coefficients.

**Modifier at AM 1.5**
  Reference air mass coefficient at 1.5 Air Mass.

*Modifier at AM 1.5 = a0 + a1 × 1.5 + a2 × 2.25 + a3 × 5.0625 + a4 × 7.59375*

**Reset to AM Defaults**
  Replaces the a0 - a4 coefficient values with default value.

Multi-Junction Cell Efficiency
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The cell efficiency table defines the cell's efficiency curve. Specify the cell efficiency at each of up to five plane-of-array (POA) beam irradiance values, and specify the reference value for capacity calculations.

SAM uses the reference value to determine the module capacity used in capacity-related cost calculations and to calculate the system's capacity factor.

During the simulation, SAM uses linear interpolation to estimate efficiency values between the points that you specify. For example, given POA Irradiance and cell efficiency values of 34% at 400 W/m2 and 36% 600 W/m2, for an hour when the plane-of-array incident radiation value is 432 W/m2, SAM would estimate the cell efficiency at 34.32%.

.. note:: Be sure that the POA irradiance values increase monotonically from top to bottom.

**POA Irradiance (W/m2, Beam Normal)**
  The beam (direct normal) radiation in the plane of the array (POA).

**Concentrated (Suns)**
  The POA Irradiance value expressed in Suns, given the concentration ratio you specify.

*Concentrated (Suns) = Concentration Ratio × POA Irradiance (W/m²) ÷ 1000 (W/m² / Sun)*

**MJ cell efficiency (%)**
  The multi-junction (MJ) cell efficiency at the given POA irradiance value.

**Reference**
  The reference POA irradiance and cell efficiency values for nameplate capacity calculations.