Heater
======

The heater parameters describe the characteristics of the electric heating equipment that supplies heat to the heat transfer fluid (HTF) to be delivered to the thermal energy storage (TES) system. The heater model currently uses a simple approach to calculate the required mass flow rate to achieve a target heat input given an inlet temperature and target outlet temperature. The model does not explicitly model the HTF pump and assumes that pumping inefficiencies are converted into heat. Future work may add heat losses to the environment and a pump model.

System Design Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

The system design parameters are variables that you specify on the :doc:`System Design <etes_system_design>` page. They are shown here for reference.

**Heater multiple**
  The heater multiple determines the heater's nominal thermal power as a multiple of the **Cycle thermal power**.

**Heater thermal power**
  The heater's thermal output under design conditions. SAM displays the heater thermal power on :doc:`Installation costs <../installation-costs/cc_etes>`   page.

*Heater Thermal Power (MWt) = Heater Multiple × Cycle Thermal Power (MWt)*

**HTF hot temperature**
  The HTF temperature at the heater outlet under design conditions.

**HTF cold temperature**
  The HTF temperature at the heater inlet under design conditions.

HTF Properties
~~~~~~~~~~~~~~

The properties of the HTF that transfers heat from the heater to the TES.

**HTF type**
  One of two types of solar salt used for the heat transfer fluid, also called the working fluid. You can also add a user defined HTF by choosing the user defined option and clicking the **Edit** button to open the HTF properties editor.

**Property table for user-defined HTF**
  When the HTF type is **User-defined**, click **Edit** to open the HTF properties editor. See :ref:`Custom HTF <etes-customhtf>`   for more about working with custom fluids.

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

.. _etes-customhtf:

Custom HTF
~~~~~~~~~~

.. include:: ../includes/snip_custom_htf.rst

