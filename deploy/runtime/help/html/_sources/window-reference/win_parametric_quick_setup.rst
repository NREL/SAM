Parametric Quick Setup
======================

This is a reference for the Parametric Quick Setup window. See :doc:`Parametric Simulations <../simulation-options/parametrics>` for details. 

Quick Setup automates the process of adding input variables to the simulation table. 

For parametric simulations with more than one variable, you can control how values are assigned to the variables in the table:

* **All combinations**: Assign all values to all variables. For example, an analysis on two variables, tilt angle with four values, and azimuth angle with five values, would result in 4 × 5 = 20 simulations.

* **Independent**: Set the value of each variable to its value on the input page and assign the values you specify in Quick Setup to the remaining variables.

* **Linked**: When variables have the same number of values, run only a single simulation for each set of values. For example, an analysis on two variables, location with three values and subarray tillt angle with three values would result in three simulations. This option is disabled when parametric variables have different numbers of values.

**Add**
  Add an input variable to the parametric variables list. You must add a variable before you can assign it multiple values.

**Remove**
  Remove a variable from the parametric variables list. When you remove a variable, SAM assigns the value from the variable's input page to the variable.

**Edit**
  Open the :doc:`Edit Parametric Values <win_edit_parametric_variables>`   window to assign values to or edit values of the variable highlighted in the parametric variables list.

**Number of simulations**
  The number of simulations that will result from the parametrics you set up. The number depends on the number of variables, values, and option (all combination, independent, linked).