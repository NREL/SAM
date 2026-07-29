
Operating costs represent annual expenditures on equipment and services that occur after the system is installed. SAM allows you to enter operating costs in three ways: Fixed annual, fixed by capacity, and variable by generation. Operating costs are reported on the project :doc:`cash flow <../results/cashflow>` in Years 1 and later.

For expenses such as component replacements that occur in particular years, you can use an annual schedule to assign costs to individual years. See below for details.

.. note:: The operating cost inputs are in Year 1 dollars. SAM applies the inflation rate from the Financial Parameters page to calculate the operating cost in each year of the cash flow. If you specify a non-zero escalation rate for an operating cost category, it applies both the inflation rate and escalation rate.

.. note:: For financial models with reserve accounts on the Financial Parameters page, you can use either the operating costs or the major equipment replacement reserve accounts for the cost of major equipment replacements.

**Fixed annual cost, $/yr**
  A fixed annual cost that applies to each year in the project cash flow.

**Fixed cost by capacity, $/kW-yr**
  A fixed annual cost proportional to the system's rated or nameplate capacity.

  The nameplate capacity kW units depend on the type of system you are modeling. For photovoltaic systems, they are DC kilowatts (kWdc). For CSP, wind, and other types of systems, they are AC kilowatts (kWac). For IPH systems, they are thermal kilowatts.

**Variable cost by generation, $/MWhac**
  A variable annual cost proportional to the system's total annual electrical output in AC megawatt-hours (or thermal output in thermal megawatt-hours for IPH systems). The annual energy output depends on either the performance model's calculated first year value and the **degradation rate** from the :doc:`Degradation <../degradation/degradation>`   page, or on an annual schedule of costs, depending on the option chosen.

**Escalation rate, %/yr**
  For each operating cost category, you can specify an optional annual **Escalation Rate** to represent an expected annual increase or decrease in operating cost above the annual inflation rate specified on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

  Specify an escalation rate of zero for an operating cost that increases annually at the rate of inflation, a positive escalation rate for a cost that increases at a higher rate than the inflation rate, or a negative escalation rate for a cost that increases at a lower rate than the inflation rate. (Set the inflation rate to the negative value of the inflation rate for a cost that does not increase with inflation.)