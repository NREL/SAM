Pumped Thermal Energy Storage
=============================

SAM's Pumped Thermal Energy Storage (PTES) model is for a system that stores electricity from the grid in both hot and cold reservoirs, using two-tank Thermal Energy Storage (TES) systems for each reservoir. A heat pump converts electricity from the grid to charge the hot tank of the hot reservoir and cold tank of the cold reservoir. A power cycle converts heat from the TES to electricity to deliver to the grid. The dispatch model charges and discharges the TES in response to electricity prices to charge from the grid when prices are low and discharge to the grid when prices are high.

The dispatch model uses a mixed-integer linear program to optimize plant operations to maximize gross profit over a look-ahead period. The model sends the optimized operations solution to a detailed performance model of the plant that ensures accurate component performance and feasible plant behavior. The dispatch model is conceptually similar to the molten salt power tower and electric thermal energy storage dispatch  models and shares common routines with them.

For a detailed description of the PTES model see:

* Neises, T.; Hamilton, B.; Martinek, J.; McTigue, J. (2022) Stand-alone and Hybrid Electric Thermal Energy Storage in the System Advisor Model. 51 pp. NREL/TP-5700-82989. (`PDF 2.3 MB <https://www.nlr.gov/docs/fy22osti/82989.pdf>`__)

