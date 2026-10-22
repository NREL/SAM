
The supercritical carbon dioxide power cycle model requires that you provide high level cycle design data

Input Value
-----------

The variables under **Input Value** are design parameters that you specify.

**Ambient temperature at design**
  The outside air temperature at the design point.

**Air cooler approach temperature**
  The difference between the ambient temperature at design and the main compressor inlet temperature. This input also applies to the pre-compressor in the partial cooling cycle. Smaller approach temperatures correspond to larger, more expensive air coolers, which you should capture in the power cycle cost.

**PHX approach temperature**
  The design point difference between the HTF hot temperature the turbine inlet temperature. Smaller approach temperatures correspond to larger, more expensive primary heat exchanger, which you could capture in the power cycle cost.

**Compressor(s) Isentropic Efficiency**
  The design point isentropic efficiency for each compressor in the cycle.

**Turbine Isentropic Efficiency**
  The design point isentropic efficiency for the turbine.

**Cooling fan electricity consumption**
  Percent of the design power cycle gross output consumed by the air cooler fan(s) at design. Smaller values will increase the annual plant generation but results in more expensive air coolers, which you should capture in the power cycle cost. For the partial cooling cycle, this value is split evenly between the two air coolers.

Value Used for Calculation
--------------------------

The variables under **Value used for Calculation** are design solution outputs.

**HTF cold temperature**
  The HTF cold temperature is governed by the cycle design solution. The power tower model will use this calculated value during the annual simulation, so it is good practice to set the corresponding variable on the System Design page to this value.

**Recompression fraction**
  The fraction of the turbine mass flow rate that bypasses the main compressor via the recompressor.

**Low pressure**
  The lowest pressure in the cycle. This is the main compressor inlet in the recompression cycle and the pre-compressor inlet in the partial cooling cycle

**Intermediate pressure**
  The main compressor inlet pressure in the partial cooling cycle. This value is not applicable for the recompression cycle.

**Low temp recuperator conductance**
  Low temperature recuperator conductance assuming a counterflow heat exchanger.

**High temp recuperator conductance**
  High temperature recuperator conductance assuming a counterflow heat exchanger. If the recompression cycle optimizes to a simple cycle configuration (i.e. recompression fraction of 0) then this value is not applicable.