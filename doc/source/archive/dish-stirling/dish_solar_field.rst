Solar Field
===========

The parameters on the Solar Field page define the size of the solar field and the layout of the dish network. To explore the impact of these parameters on the system's costs and performance, change the value of the parameter.

.. include:: ../includes/snip_dish_references.rst

* Section 3.1, p 63

* Appendix A, p 152

Field Layout
~~~~~~~~~~~~

The solar field is assumed to be a rectangular field with collectors oriented north-south and east-west.

**Number of Collectors North-South**
  Number of collectors oriented along north-south lines. Used to calculate the total number of collectors.

**Number of Collectors East-West**
  Number of collectors oriented along east-west lines. Used to calculate the total number of collectors.

**Number of Collectors**
  Total number of collectors in the field. Used to calculate the predicted system output, the shading factor, and piping distance for pumping loss calculation.**Collector Separation North-South (m)**
  Center-to-center distance between collectors along north-south lines. Used to calculate the solar field area, shading factor, and piping distance for pumping loss calculation.

**Collector Separation East-West (m)**
  Center-to-center distance between collectors along east-west lines. Used to calculate the solar field area, shading factor, and piping distance for pumping loss calculation.

**Total Solar Field Area (m****\ :sup:`2`\****)**
  The total ground area occupied by the collectors. Used in area-related cost calculations.

System Properties
~~~~~~~~~~~~~~~~~

**Wind Stow Speed (m/s)**
  When the wind velocity from the weather file for the current hour is greater than or equal to this value, the concentrator moves into stow position to prevent wind damage. The solar power intercepted by the receiver is zero during this hour.**Total Solar Field Capacity (kWe)**
  Nominal electric output capacity of the solar field. Used in capacity-related cost calculations.

Array Shading Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

SAM uses the shading parameters to calculate the shading of the concentrator mirror by the dish components and by neighboring dish systems. SAM's approach to modeling shading is different from the Osborn approach described in the Fraser thesis.

**Ground Slope, North-South (%)**
  Slope of the ground in percent (rise over run) along a north-south line. A positive slope indicates that for two dishes aligned north-south, the dish to the south is lower than the dish to the north. Used to calculate shading factor.

**Ground Slope,East-West (%)**
  Slope of the ground in percent (rise over run) along a east-west line. A positive slope indicates that for two dishes aligned east-west, the dish to the east is lower than the dish to the west. Used to calculate shading factor.

**Slot Gap Width (m)**
  Average width of the slot in the mirror perpendicular to the vertical support post. Used to calculate shading factor.

**Slot Gap Height (m)**
  Average height of the slot in the mirror parallel to the vertical support post. Used to calculate shading factor.

Equations for Calculated Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Number of Collectors
--------------------

The total number of collectors is calculated based on the numbers of north-south and east-west oriented collectors.

.. image:: ../images/EQ_N-coll.png
   :align: center
   :alt: EQ_N-coll.png

Where,

.. list-table::
   :width: 100%
   :align: center

   * - *N*\ :sub:`Coll`\ 
     - Number of Collectors
   * - *N*\ :sub:`Coll,N-S`\ 
     - Number of Collectors North-South
   * - *N*\ :sub:`Coll,E-W`\ 
     - Number of Collectors East-West

Total Solar Field Area
----------------------

The total solar field area is the product of the north-south and east west dish collector separation distances and the number of collectors.

.. image:: ../images/EQ_A-SF.png
   :align: center
   :alt: EQ_A-SF.png

Where,

.. list-table::
   :width: 100%
   :align: center

   * - *A*\ :sub:`SF`\  (m\ :sup:`2`\ )
     - Total Solar Field Area
   * - *d*\ :sub:`CollSep,N-S`\  (m)
     - Collector Separation North-South
   * - *d*\ :sub:`CollSep,E-W`\  (m)
     - Collector Separation East-West
   * - *N*\ :sub:`Coll`\ 
     - Number of Collectors

Total Capacity
--------------

The total solar field capacity is the product of the number of collectors and the engine nameplate capacity on the Stirling Engine page.

.. image:: ../images/EQ_P-SF.png
   :align: center
   :alt: EQ_P-SF.png

Where,

.. list-table::
   :width: 100%
   :align: center

   * - *P*\ :sub:`SF`\  (W)
     - Total Capacity
   * - *P*\ :sub:`Engine`\  (W)
     - Singe Unit Nameplate Capacity from the :doc:`Stirling Engine page <dish_stirling_engine>`.
   * - *N*\ :sub:`Coll`\ 
     - Number of Collectors