Electric HTF Heater
===================

The heater parameters describe the characteristics of optional electric heating equipment that supplies heat to the cold heat transfer fluid (HTF). SAM assumes the heater operates in parallel with the tower and receiver, heating cold HTF from the TES and delivering hot HTF to the TES.  The heater model currently uses a simple approach to calculate the required mass flow rate to achieve a target heat input given an inlet temperature and target outlet temperature. The model does not explicitly model the HTF pump and assumes that pumping inefficiencies are converted into heat.

To enable the electric HTF heater inputs, check **Enable electric heater to charge cold HTF** on the :doc:`System Design <mspt_system_design>` page.

System Design Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

The system design parameters are variables that you specify on the :doc:`System Design <../electric-thermal-energy-storage/etes_system_design>` page. They are shown here for reference.

**Heater multiple**
  The heater multiple determines the heater's nominal thermal power as a multiple of the **Cycle thermal power**.

**Heater thermal power**
  The heater's thermal output under design conditions. SAM displays the heater thermal power on :doc:`Installation costs <../installation-costs/cc_etes>`   page.

*Heater Thermal Power (MWt) = Heater Multiple × Cycle Thermal Power (MWt)*

**HTF hot temperature**
  The HTF temperature at the heater outlet under design conditions.

**HTF cold temperature**
  The HTF temperature at the heater inlet under design conditions.

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