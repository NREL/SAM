Fuel Cell Dispatch
==================

The fuel cell Dispatch page provides options for defining how to operate the fuel cell stack and battery bank.

The battery storage dispatch options available depend on whether the system is in front of the electric utility meter (PPA Single Owner financial model) or behind the meter (Commercial financial model).

Fuel Cell Operation
~~~~~~~~~~~~~~~~~~~

The fuel cell operation parameters determine the fuel cell operates.

Dispatch Options
----------------

**Fixed output**
  Operate the fuel cell at a fixed percentage of the fuel cell rated capacity at all times.

**Fixed output percentage**
  For the fixed output option, specify the percentage of the fuel cell rated capacity from the :doc:`Fuel Cell <fuelcell_fuel_cell>`   page. This input is disabled unless you choose the **Fixed output** dispatch option.

**Follow electric load**
  Operate the fuel cell to follow the electric load defined on the :doc:`Electric Load <../electricity-rates-and-load/electricity_load>`   page.

.. note:: This option does not apply to the front of meter configuration (PPA single owner financial model).

**Manual dispatch**
  Operate the fuel cell according to the **Manual Dispatch** parameters and schedules.

.. note:: The fuel cell manual dispatch option requires that you enable the battery on the Battery Storage even if the system does not include a battery. If the system does not include a battery, choose Enable Battery on the Battery Storage page, choose Set desired bank size, and set Desired bank power and Desired bank capacity both to zero to effectively disable the battery and enable manual dispatch. You can also follow the prompts when you choose manual dispatch to have SAM do that automatically.

**Input dispatch**
  Operate the fuel cell according to a time series dispatch profile that you provide.

**Input dispatch schedule**
   For the input dispatch option, click **Edit array** to provide a time series array of either kW or percentage values at which to operate the fuel cell in each simulation time step.

Operation Options
-----------------

**Allowed to shut down**
  Allow the fuel cell to shut down completely when it is not not needed to meet dispatch requirements.

**Idle at minimum power**
  Run the fuel cell at the minimum unit output from the :doc:`Fuel Cell <fuelcell_fuel_cell>`   page when it is not needed to meet dispatch requirements.


