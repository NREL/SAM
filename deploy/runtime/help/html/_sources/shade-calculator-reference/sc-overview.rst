3D Shade Calculator
===================

The SAM 3D Shade Calculator uses a sun position algorithm and a three-dimensional drawing of a photovoltaic array and nearby shading objects to generate hour-by-month tables of beam irradiance shade loss percentages, and a sky diffuse loss percentage. It has the following features:

* Uses Bing Maps geocoding to automatically find latitude, longitude, and time zone for any street address.

* A three dimensional drawing, or shade scene, represents the photovoltaic array and nearby shading objects.

* The scene consists of active surfaces (photovoltaic subarrays) and shading objects.

* Four three-dimensional shapes are designed to represent most shading objects: Box, cylinder, tree, and roof.

* View and edit the shade scene in three-dimensional, bird's eye, and elevation views.

* Add an optional aerial photograph from Bing Maps or your own image as an underlay for the bird's eye view to help position objects on the scene.

* Drag the three-dimensional view to see shadows on active surfaces and the shade loss at different sun positions.

* Calculate diurnal and hourly beam irradiance shade loss for a single year based on the scene location and relative positions of each active surface and shading objects in the scene.

To use the SAM shade analysis calculator:

#. On the :doc:`../detailed-photovoltaic-model/pv_soiling_shading_snow` page (detailed photovoltaic model) or :doc:`System Design <../pvwatts/pvwatts_system_design>` page (PVWatts), click **Open 3D shade calculator**.

#. :doc:`Define the scene location <../shade-calculator-howto/sc-define-location>` (latitude, longitude, time zone).

#. :doc:`Add shading objects <../shade-calculator-howto/sc-add-shading-objects>` to the scene. These are trees, buildings, and other structures near the array that may block direct solar irradiance from reaching the array.

#. :doc:`Add active surface <../shade-calculator-howto/sc-active-surface>` objects to the scene to represent the photovoltaic array or subarrays.

#. Save and close the calculator to apply shading factor values to the SAM :doc:`../detailed-photovoltaic-model/pv_soiling_shading_snow` inputs.

#. :doc:`Generate shade data <../shade-calculator-howto/sc-generate-data>`.

#. :doc:`Export shade data <../shade-calculator-howto/sc-export>` to use in SAM or other simulation software.

For more step-by-step instructions, see The How-To section.

For a description of the 3D shade calculator's features, see the Reference section.