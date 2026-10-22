
Curtailment payments generate revenue in addition to energy payment or PPA revenue during times when the project is required to reduce or stop electricity generation by the electricity off-taker.

Use the grid curtailment inputs on the :doc:`Grid <../grid/grid_limits>` page to define the curtailment schedule.

Curtailment Compensation
------------------------

**Curtailed energy compensation, $/kWh**
  The compensation rate in dollars per kWh of curtailed energy.

  You can specify a single compensation rate as a Year 1 value with optional escalation rate, or click **Sched** and **Edit** to specify a compensation rate for each year using the :doc:`Edit Schedule <../window-reference/win_edit_data_table_column>`   window.

  .. image:: /images/SS_AnnSched-SchedEdit.png
     :align: center
     :alt: SS_AnnSched-SchedEdit.png

  For each hourly or subhourly time step, the curtailed energy is the system's power output minus the curtailment limit from the curtailment table on the :doc:`Grid <../grid/grid_limits>`   page. The annual curtailed energy is the sum of the hourly or subhourly curtailed power values over one year. SAM shows the annual curtailed energy amount for each year in the project :doc:`cash flow <../results/cashflow>`  .

**Curtailed compensation escalation, %/year**
  The curtailment compensation escalation rate applies to the curtailment revenue in Years 2 and later when you specify the compensation rate as a single Year 1 value. SAM does not apply the inflation rate from the Financial Parameters page to the curtailment compensation rate.