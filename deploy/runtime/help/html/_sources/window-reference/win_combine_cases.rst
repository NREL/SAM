Combine Cases
=============

The Combine Cases window allows you to choose cases to treat as a combined system. SAM calculates a time series generation profile by adding up the Year 1 power generation data for each case that you choose to use as input to either the Custom Generation Profile or Custom Generation Profile - Battery model. This makes it possible to model a project that involves a combination of different kinds of systems.

**Select cases**
  Choose the cases you want to combine. This requires that your SAM file have one Custom Generation Profile or Custom Generation Profile - Battery case and at least two other cases. For example, you could use a file with a Custom Generation Profile, Detailed PV, and Wind case to combine the output of a PV and wind system.

**Overwrite installation and operating costs with combined cases costs**
  Choose this option if you want SAM to replace the total installed cost and operating costs inputs with costs calculated by adding up the costs in the different cases.

**Annual AC degradation rate for all cases, %/year**
  Enter a value for the annual degradation rate to use for the combined system. Combine Cases only considers the first year of data from each case. The degradation rate will account for any degradation you expect in the combined system output from year to year.