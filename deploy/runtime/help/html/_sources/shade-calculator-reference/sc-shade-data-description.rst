Shade data description
======================

The shade loss at a given time is the ratio of the shaded area of the :doc:`active surface <sc-create>` to its total area expressed as a percentage. The beam shade factor is one minus the shade loss expressed as a fraction.

The shade loss represents a reduction in beam solar irradiance incident on the active surface. The shade loss algorithm does not estimate the reduction in diffuse solar irradiance due to reduction of the view factor caused by shade objects.

If there is more than one active surface in the scene, you can :doc:`generate <../shade-calculator-howto/sc-generate-data>` a diurnal table for groups of one or more surfaces, or for groups of active surfaces. You can only generate a single hourly table for the active surfaces combined into a single array -- you cannot generate multiple hourly tables.

The basic shade loss algorithm is:

#. Calculate the sun position given the scene's latitude, longitude, and timezone

* For the diurnal table, the sun position is at the midpoint of each hour on the 14th day of each month.

* For the hourly table, the sun position is at the midpoint of each hour of the year.

#. Transform the 3D scene so that the view is aligned to the perspective of the sun.

#. Calculate coordinates of polygons representing the scene.

#. Cull and sort the polygons in order along the vector from the scene to the sun and projected onto a 2D plane. (This step assumes parallel sun rays, so it uses a parallel projection rather than a perspective projection).

#. For each active surface, calculate coordinates of shade polygons, which are polygons that intersect the active surface and shade object polygons. 

#. Calculate the shade loss as the ratio of shade polygon area to the active surface area expressed as a percentage.