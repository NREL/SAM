Heat Pump
=========

The heat pump parameters describe the characteristics of the heat pump that supplies heat to the hot heat transfer fluid (HTF) to be delivered to the hot thermal energy storage (TES) reservoir and cools the cold HTF to be delivered to the cold TES reservoir. The heat pump currently applies a simple approximation  to calculate off-design performance as reservoir temperatures and load change. Future work may add more detailed off-design models or allow users to input off-design data. 

For a detailed description of the PTES model see:

* Neises, T.; Hamilton, B.; Martinek, J.; McTigue, J. (2022) Stand-alone and Hybrid Electric Thermal Energy Storage in the System Advisor Model. 51 pp. NREL/TP-5700-82989. (`PDF 2.3 MB <https://www.nlr.gov/docs/fy22osti/82989.pdf>`__)

System Design Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

The system design parameters are variables that you specify on the :doc:`System Design <ptes_system_design>` page. They are shown here for reference.

**Heat pump thermodynamic power, MWe**
  Net power imparted on the heat pump working fluid. Additional power consumption like electric motor inefficiency, cooling parasitics, and HTF pumping power are captured in separate inputs on the Heat Pump page and reflected in **Total electricity consumption at design charge** and **Net round-trip efficiency**.

**Hot storage cold temperature, °C**
  Temperature of the cold tank of the hot reservoir at design. This is also the temperature of the hot HTF entering the heat pump hot heat exchanger at design and the temperature exiting the cycle hot heat exchanger at design.

**Hot storage hot temperature, °C**
  Temperature of the hot tank of the hot reservoir at design. This is also the temperature of the hot HTF exiting the heat pump hot heat exchanger at design and the temperature entering the cycle hot heat exchanger at design.

**Heat pump heat out, MWt**
  The heat pump thermal output to the Hot storage hot tank under design conditions.

**Cold storage cold temperature, °C**
  Temperature of the cold tank of the cold reservoir at design. This is also the temperature of the cold HTF exiting the heat pump cold heat exchanger at design and the temperature entering the cycle cold heat exchanger at design.

**Cold storage hot temperature, °C**
  Temperature of the hot tank of the cold reservoir at design. This is also the temperature of the cold HTF entering the heat pump cold heat exchanger at design and the temperature exiting the cycle cold heat exchanger at design.

Component Parameters
~~~~~~~~~~~~~~~~~~~~

The properties of the HTF that transfers heat from the heater to the TES.

Startup and Operation
---------------------

**Fraction of design power allowed during startup**
  Fraction of the design heat pump thermal power output allowed to bring the system to operating temperature after a period of non-operation.

**Duration of startup at max startup power, hr**
  The amount of time at the maximum allowed thermal power at startup that is required to reach operating temperature during startup.

**Startup energy, MWt-hr**
  The thermal energy required during the startup period. Electric energy is calculated by applying the design point heat pump coefficient of performance.

*Startup energy (MWt-hr) = Heat Pump Heat Out (MWt) × Fraction of Design Power Allowed During Startup × Duration of Startup at Max Startup Power (h)*

**Minimum operating fraction**
  The minimum heat pump thermal power output allowed as a fraction of the design point.

Heat Pump Parasitics
--------------------

**Parasitics (non-pumping) as fraction of thermodynamic power**
  Electrical consumption of heat pump parasitics, not including HTF pumping power, defined as a fraction of the design heat pump net thermodynamic power input. Examples of potentially relevant parasitics include electric motor inefficiencies and heat rejection parasitics. At off-design this value is calculated by scaling by the ratio of off-design thermodynamic power to design thermodynamic power.

**Parasitics, MWe**
  Calculated electrical consumption of heat pump parasitics at design conditions, not including HTF pumping power

Hot HTF Pumping Power
---------------------

**Pumping power rate through hot heat exchanger, kWe/kg/s**
  Electrical power required to move 1 kg/s of hot HTF through the hot side heat exchanger.

**Hot HTF mass flow rate, kg/s**
  Hot HTF mass flow rate at design conditions.

**Hot HTF pumping power, MWe**
  Electrical power required to pump hot HTF at design conditions.

Cold HTF Pumping Power
----------------------

**Pumping power rate through cold heat exchanger, kWe/kg/s**
  Electrical power required to move 1 kg/s of cold HTF through the cold side heat exchanger.

**Cold HTF mass flow rate, kg/s**
  Cold HTF mass flow rate at design conditions.

**Cold HTF pumping power, MWe**
  Electrical power required to pump cold HTF at design conditions.

Net Metrics
-----------

**Total heat pump power in, MWe**
  Total heat pump power consumption calculated by adding electrical parasitics to the heat pump thermodynamic power.

**Net efficiency**
  Heat pump coefficient of performance calculated using **Total heat pump power in**.

Startup
~~~~~~~

**Fraction of design power allowed during startup**
  Fraction of the design heater thermal power allowed to bring the system to operating temperature after a period of non-operation. Used by the dispatch module to calculate the required start-up energy.

**Duration of startup at max startup power, hr**
  The amount of time required to reach operating temperature during startup.

**Heater startup energy, MWt-hr**
  The energy required during the startup period.

*Heater Startup energy (MWt-hr) = Heater Thermal Power (MWt) × Fraction of Design Power Allowed During Startup × Duration of Startup at Max Startup Power (h)*

**Minimum heater operating fraction**
  The minimum heater power allowed as a fraction of the heater thermal power.