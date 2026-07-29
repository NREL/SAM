Edit Parametric Values
======================

The Edit Parametric Values window helps you define a range of values using a start value, end value, and increment for :doc:`parametric simulations <../simulation-options/parametrics>`.

To open the Edit Parametric Values window:

1. Click **Parametrics** under the Simulate button.

2. Click **Quick Setup** to open the :doc:`Parametric Quick Setup window <win_parametric_quick_setup>`.

3. Click **Add** to add a variable, and then click **Edit** or double-click the variable name.

To define a range of values for a parametric variable:

#. Under **Define range**, type a start value, for example 5.

#. Type an end value, for example 10.

#. Type an increment, for example 1.

#. Click **Update**.

The result under **Variable values** should be a range of values, for this example, the range would be: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10.

Regular pattern example: To assign the values 0, 2.5, 5, 7.5, 10 to a variable:

#. Type 0 for the start value.

#. Type 10 for the end value.

#. Type 2.5 for the increment.

#. Click **Update**.

To assign values that do not follow a regular pattern, you can use the buttons under the Variable Values box to add values to the list. 

Irregular range example: To assign the values 0, 3, 4, 5, 6, 10 to a variable:

#. Type 3 for the start value.

#. Type 6 for the end value.

#. Type 1 for the increment.

#. Click **Update**.

#. Select the number 3 in the Variable Values list.

#. Click **Add Before**.

#. Type 0 in the Add Before Selection window.

#. Click **OK**.

#. Select the number 6 in the Variable Values list.

#. Click **Add After**.

#. Type 10 in the Add After Selection window.

#. Click **OK**.

Use the Up, Down, and Remove buttons to move through the list of variable values and to delete values.

Variable Values
---------------

The Variable Values list shows the values that SAM will assign to the parametric variable or slider.

**Add After**
  Add a value after the selected value in the Variable Values list.

**Add Before**
  Add a value before the selected value in the list.

**Up**
  Move the selected value one step up in the list.

**Down**
  Move the selected value one step down in the list.

**Remove**
  Remove the selected value from the list.

Define Range
------------

**Start Value**
  The range of variable values displayed in the Variable Values list begins with this value. The start value units and magnitude should be appropriate for the variable to which values are being assigned.

**End Value**
  Range of variable values ends with this value. The start value units and magnitude should be appropriate for the variable to which values are being assigned.

**Increment**
  SAM adds the increment value to each value in the list to calculate the next value. Increment values may be positive, negative, or decimal values.

**Update**
  Calculates values based on the parameters defined above and displays them in the Variable Values list.