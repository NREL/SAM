Electric Thermal Energy Storage)
================================

SAM's Electric Thermal Energy Storage (ETES) model is for a system that stores electricity from the grid in a two-tank Thermal Energy Storage (TES) system. An electric heater converts electricity from the grid to heat to charge the TES. A power cycle converts heat from the TES to electricity to deliver to the grid. The dispatch model charges and discharges the TES in response to power prices to charge from the grid when prices are low and discharge to the grid when prices are high.

The dispatch model uses a mixed-integer linear program to optimize plant operations to maximize gross profit over a look-ahead period. The model sends the optimized operations solution to a detailed performance model of the plant that ensures accurate component performance and feasible plant behavior. The dispatch model is conceptually similar to the molten salt power tower dispatch model and shares common routines with it.

.. image:: ../images/IMG_ETES-system-design-diagram.png
   :align: center
   :alt: IMG_ETES-system-design-diagram.png

