Simulation
==========

A simulation is the set of calculations made by the performance and financial models to calculate :doc:`results <../results/results>`. The performance model makes a one set of calculations for each time step in the :doc:`weather file <../weather-data/weather_data>` (8,760 time steps for an hourly weather file), and the financial model makes another set of calculations for each year in the analysis period that you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page. 

You can :doc:`run simulations <../getting-started/run_simulations>` from SAM, or from a :doc:`macro or script <../reference/macros>`.

Multiple Simulations
~~~~~~~~~~~~~~~~~~~~

Some analysis scenarios require more than one simulation. For example, a parametric analysis requires running simulations for a range of values for one or more input variables. SAM offers three options for multiple simulations:

* A :doc:`parametric <parametrics>` simulation allows you to assign multiple values to one or more input variables and create graphs showing the dependence of a result on those variables. For example, you can use a parametric analysis to create a graph of a photovoltaic system's annual electricity output versus the array tilt and azimuth angles, or, for a parabolic trough system, you can create a graph of levelized cost of energy (LCOE) versus solar multiple to determine the optimal solar field size.

* A :doc:`stochastic <stochastic>` analysis is similar to a parametric analysis, except that SAM automatically generates values of input variables over a range and probability distribution that you specify. 

* A :doc:`P50/P90 <p50p90>` analysis involves running a simulation for a set ten or more single-year weather files for consecutive years, and then sorting the annual electrical output and other metrics in order of increasing occurrence over the time period. You can use that information to estimate the certainty of SAM's output predictions given your assumptions.

You can use SAM to optimize the value of one or more inputs using one of the simulation tools listed above as described in :doc:`Optimization <optimization>`.

You can also design your own models that require multiple simulations by writing a :doc:`script <../reference/macros>` in SAM, or you can call the SAM simulation core (SSC) outside of SAM with the :doc:`software development kit <../reference/sdk>`.