Battery Time Step
=================

For standalone batteries, SAM is unable to determine the simulation time step because there is no weather file or generation profile. The simulation time step input allows you to set the simulation time step.

.. note:: If you choose **Dispatch to custom time series** on the Battery Dispatch page for FOM batteries. or **Input battery power targets** for BTM batteries, the time step of the time series battery power targets must be the same as the simulation time step.

**Simulation time step**
  The simulation time step in minutes for the standalone battery. Allowed values are 1, 5, 15, 20, 30, and 60 minutes.