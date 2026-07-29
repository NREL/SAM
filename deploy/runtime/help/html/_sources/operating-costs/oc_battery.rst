Battery Operating costs
=======================

Operating costs represent annual expenditures on equipment and services that occur after the system is installed.

The inputs that appear on the Operating costs page depend on whether you are using the Standalone Battery,  Custom Generation Profile-Battery or a hybrid system configuration.

.. note:: The operating cost inputs are in Year 1 dollars. SAM applies the inflation rate from the Financial Parameters page to calculate the operating cost in each year of the cash flow. If you specify a non-zero escalation rate for an operating cost category, it applies both the inflation rate and escalation rate.

   For financial models with reserve accounts on the Financial Parameters page, you can use either the operating costs or the major equipment replacement reserve accounts for the cost of major equipment replacements.

   For expenses such as component replacements that occur in particular years, you can use an :ref:`annual schedule <oc-battery-annualschedule>` to assign costs to individual years.

**Nameplate capacity**
  The nominal storage capacity of the battery in DC kilowatt-hours from the Battery Cell and System page.

  For the Custom Generation Profile - Battery configuration, the system capacity in AC kilowatts is from the :doc:`Generation Profile <../custom-generation/custom_generation_profile>`   page.

**Fixed annual cost, $/yr**
  A fixed annual cost that applies to each year in the project cash flow.

**Fixed cost by capacity**
  A fixed annual cost per unit of nameplate capacity as defined above.

**Variable cost by generation**
  A variable annual cost proportional to the annual output of the photovoltaic system (MWac) or power plant (MWac), and annual energy discharged by the battery (MWac). The variable operating cost of the battery is calculated from the total energy discharged to the grid and load (for behind-the-meter batteries).

**Replacement cost, $/kWhdc of battery nameplate capacity**
  The replacement cost in Year 1 dollars per unit of nameplate battery capacity as defined above.

  .. note:: To include battery replacements costs in your analysis, be sure to choose a battery replacement option on the :doc:`Battery Life <../battery-storage/battery_life>` page.

**Escalation rate, %/yr**
  For each operating cost category, you can specify an optional annual **Escalation Rate** to represent an expected annual increase or decrease in operating cost above the annual inflation rate specified on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

  Specify an escalation rate of zero for an operating cost that increases annually at the rate of inflation, a positive escalation rate for a cost that increases at a higher rate than the inflation rate, or a negative escalation rate for a cost that increases at a lower rate than the inflation rate. (Set the inflation rate to the negative value of the inflation rate for a cost that does not increase with inflation.)

Land Lease costs
~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_operating_cost_land_lease.rst

.. _oc-battery-annualschedule:

Using Annual Schedules to Specify Operating costs in Specific Years
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_o_and_m_periodic_costs.rst

