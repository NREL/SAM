Collectors (SCAs)
=================

A solar collector assembly (SCA) is an individually tracking component of the solar field that includes mirrors, a supporting structure, and heat collection elements or receivers.

For a more detailed description of the model, please download the CSP trough reference manual from the `SAM website <https://sam.nlr.gov/concentrating-solar-power/csp-publications>`__.

Solar Collector Assembly (SCA)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The solar collector assembly (SCA) input variables describe the dimensions and optical characteristics of the SCA or collector.

**Current SCA inputs**
  The name of the collector in the SCA library

**SCA Length (m)**
  The total length of a single SCA. Used in SCA end loss calculation.

**SCA Aperture (m)**
  The structural width of a single SCA, including reflective area and gaps. Used in the row-to-row shadowing loss factor and HCE thermal loss calculations.

**SCA Aperture Reflective Area (m****\ :sup:`2`\****)**
  The reflective area of a single SCA, not including gaps. Used in the solar field size calculations.

**Average Focal Length (m)**
  Average trough focal length. Used in end gain and end loss factor calculations.

**Incident Angle Modifier Coef F0, F1, F2**
  Incident angle modifier coefficients. Used to calculate the incident angle modifier factor, which is used to calculate the HCE absorbed energy and the solar field optical efficiency.

**Tracking Error and Twist**
  Accounts for errors in the SCA's ability to track the sun. Sources of error may include poor alignment of sun sensor, tracking algorithm error, errors caused by the tracker drive update rate, and twisting of the SCA end at the sun sensor mounting location relative to the tracking unit end. A typical value is 0.985. Used to calculate SCA field error factor. 

**Geometric Accuracy**
  Accounts for SCA optical errors caused by misaligned mirrors, mirror contour distortion caused by the support structure, mirror shape errors compared to an ideal parabola, and misaligned or distorted HCE. A typical range of values is between 0.97 and 0.98. Used to calculate SCA field error factor.

**Mirror Reflectance**
  The mirror reflectance input is the solar weighted specular reflectance. The solar-weighted specular reflectance is the fraction of incident solar radiation reflected into a given solid angle about the specular reflection direction. The appropriate choice for the solid angle is that subtended by the receiver as viewed from the point on the mirror surface from which the ray is being reflected. For parabolic troughs, typical values for solar mirrors are 0.923 (4-mm glass), 0.945 (1-mm or laminated glass), 0..906 (silvered polymer), 0.836 (enhanced anodized aluminum), and 0.957 (silvered front surface).

**Mirror Cleanliness Factor (avg)**
  Accounts for dirt and dust on the mirrors that reduce their effective reflectivity. Typically, mirrors are continuously cleaned, but a single mirror may be cleaned once each one or two weeks. The expected overall effect on the total solar field would be an average loss of between one and two percent. A typical value would be 0.985. Used to calculate SCA field error factor.

**Dust on Envelope (avg)**
  Accounts for dust on the HCE envelope that affects light transmission. A typical value would be 0.99. Used to calculate HCE heat loss.

**Concentrator Factor**
  A additional error factor to make it possible to adjust the SCE performance without modifying the other error factors. Useful for modeling an improved or degraded SCE. The default value is 1. Used to calculate SCA field error factor.

**Solar Field Availability**
  Accounts for solar field down time for maintenance and repairs. Used to calculate absorbed energy.

.. _scahce_aboutthescaparameters:

About the SCA Parameters
~~~~~~~~~~~~~~~~~~~~~~~~

The default SCA library includes a set of parameters for four types of SCAs described in the table below. These SCA types are either installed in currently operating systems, or were used in past system designs. See :doc:`Working with Libraries <../reference/libraries>` for information about managing libraries.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Name
     - Description
     - Location
   * - Euro Trough ET150
     - Torque box, galvanized steel
     - SEGS V, Kramer Junction, California
   * - Luz LS-2
     - Torque-tube, galvanized steel
     - SEGS I - VII, Kramer Junction, California
   * - Luz LS-3
     - Bridge truss, galvanized steel
     - SEGS VII - IX, Kramer Junction, California
   * - Solargenix SGX-1
     - Organic hubbing structure, extruded aluminum
     - Nevada Solar One, Boulder City, Nevada

The values of input variables on the SCA / HCE page are stored in libraries. See :doc:`Working with Libraries <../reference/libraries>` for information about managing libraries.

.. _scahce_aboutthemirrorreflectivityvalue:

About the Mirror Reflectance Value
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following information is intended to help choose a value for the mirror reflectance factor. The solar weighted hemispherical reflectance (SWV) of mirror glass depends on the iron content, thickness, and tempering of the glass, and the thickness of the reflective coating of the mirror:

* Glass transmittance and mirror reflectivity both depend on the iron (Fe2O3) content of the glass. The higher the iron content, the lower the transmittance and the higher the reflectivity of the mirror. Iron contents of more than 0.02% typically result in unacceptably low mirror reflectivity values. 

* Mirror reflectance increases as glass thickness decreases. The thinner glass requires faster pulling during manufacturing and is easier to break during shipping and handling than thicker glass. A glass thickness of one millimeter mounted with a substrate is a reasonable compromise to maximize mirror reflectivity and minimize the risk of mirror breakage. Five millimeter thick, non-tempered, low-iron, self-supporting glass mirrors are typically recommended for mirrors at the periphery of the parabolic trough field that are exposed to wind. Normally, five to ten percent of a solar field is equipped with 5 mm glass. 

* Glass tempering generally raises mirror reflectivity.

* Mirror coating typically uses a silver thickness between 800 - 1200 Å or 0.8 -1.2 g/m2. Silver thicknesses less than 0.8 g/m2 result in unacceptably low mirror reflectivity values. Silver thicknesses greater than 1.2 g/m2 do not improve reflectivity, and have a tendency to delaminate.

**Table ****. Suggested mirror reflectivity values for different types of commercially available glass solar mirrors using pristine second surface glass.**

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Glass Thickness (mm)
     - Iron Content
     - Mirror Reflectance
   * - 4
     - low 
     - 0.93 ±0.002
   * - 1
     - low
     - 0.96 ±0.002
   * - 4
     - low
     - 0.948 ±0.003
   * - 4
     - very low
     - 0.946 ±0.001
   * - 3
     - very low
     - 0.956 ±0.001

