
SAM's bifacial model is adapted from Marion, B, et. al (2017) A Practical Irradiance Model for Bifacial PV Modules, available for download from the `SAM website <https://sam.nlr.gov/photovoltaic/pv-publications.html>`__. The model's open source implementation is available on the NLR GitHub website at https://github.com/NatLabRockies/bifacialvf. The bifacial model calculates the plane-of-array irradiance on the rear side of the PV modules in the array assuming infinite rows. The rear-side plane-of-array irradiance is multiplied by the module bifaciality factor, and then added to the front-side plane-of-array irradiance, which is then sent to the module model to predict module DC power. Primary drivers of the rear-side POA irradiance include the ground albedo, row-to-row spacing (function of ground-coverage ratio), and ground clearance height.

**Module is bifacial**
  Check this box to enable the bifacial model and its inputs.


 
.. note:: The CEC module library indicates whether the module is bifacial. You can sort the library table by clicking the Bifacial column heading: Bifacial modules have a value of one.


.. note:: When you enable the bifacial model, SAM enables inputs for rear-side irradiance and bifacial electrical mismatch losses on the :doc:`Losses <../detailed-photovoltaic-model/pv_electrical_losses>` page, and provide an option for spatial albedo values on the Location and Resource page.

**Transmission Fraction**
  The ratio of area of each row that allows light to reach the surface behind the row to the area that blocks light, expressed as a fraction between 0 and 1. The row area is the total area occupied by the modules in a single row, including space between the modules. Light passes through space between modules, and, for modules made of glass or other transparent material, may also pass through the space between cells in each module.

  For an array of opaque modules:

*Transmission Factor = ( Total Row Area (m²) - Row Opaque Area (m²) ) ÷ Total Row Area (m²)*

  For opaque modules, the row opaque area is the product of the number of modules in one row and the module area. For modules with transparent backs, the row opaque area is the total cell area in one row plus the area of the frame and other material that may block light.

.. note:: The transmission fraction determines the amount of light that can pass around or through modules in a single row. The GCR (ground coverage ratio) on the System Design page and array geometry on the Shading and Layout page determine the spacing between rows.

**Bifaciality**
  The ratio of rear-side efficiency to front-side efficiency. Typically a value between 0.65 and 0.9 provided on the bifacial module datasheeet. This is to account for the fact that photovoltaic cells on the rear of the module are usually less efficient than the cells on the front of the module. The bifaciality does not affect the solar irradiance on the rear of the module.

**Ground clearance height**
  The height from the ground to the bottom of the PV array. For systems with no tracking, it is the distance between the ground and the bottom edge of modules closest to the ground. For systems with tracking, it is the distance between the ground and back of modules when they are horizontal.