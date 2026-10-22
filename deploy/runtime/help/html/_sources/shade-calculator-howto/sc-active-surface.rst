Add active surface objects
==========================

An :doc:`active surface <../shade-calculator-reference/sc-create>` object is a shape that represents the photovoltaic array or subarray. You can add as many active surface objects to the scene as you like.

The measurements for the size properties of objects (length, width, height, diameter) in the scene can be in any units, as long as you use the same units for all measurements to ensure that the relative size and position of the objects is consistent.

An active surface object is the shaded object. When you :doc:`generate shade data <sc-generate-data>`, the shade calculator calculates the fraction (expressed as a percentage) of each active surface that is shaded by the :doc:`shading objects <../shade-calculator-reference/sc-shading-objects>` and other active surfaces.

.. note:: The active surface object is intended to represent a small rooftop array or subarray. It is not designed to represent a row in a large roof- or ground-mounted array that consists of many rows of modules.

To add an active surface object

#. Click **3D scene**, **Bird's eye**, or **Elevations** to select a view.

#. Click **Create**, and choose **Active Surface** from the list.

The active surface will appear at the origin of the scene coordinates.

#. :doc:`Move and resize the object <sc-move>` in bird's eye or elevations view.

#. Type a value for **Tilt** and **Azimuth** in the surface's properties. These correspond to the photovoltaic array's tilt and azimuth values.

#. If your scene has more than one active surface object, and you want to generate separate diurnal shade data tables, assign a different **Group** name to each surface. See :ref:`Group <group>` for details.

Scene with Four Active Surface Objects

.. image:: ../images/ss-active-surface-objects.png
   :align: center
   :alt: ss-active-surface-objects.png

