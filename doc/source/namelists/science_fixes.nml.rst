``science_fixes.nml``
=====================

This file contains one namelist called :nml:lst:`JULES_TEMP_FIXES`.

This namelist sets 'short-term' temporary logicals used to protect science bug
fixes that lead to alterations in science results. It is expected that these
logicals will be short lived as the preference should be for all configurations
to use the corrected code. However, to maintain short term reproducibility of
results across JULES versions the fixes are protected by logicals until the
fixes become the default in all model configurations at which point the logical
is retired. See module for when the switch is due for review.

``JULES_TEMP_FIXES`` namelist members
-------------------------------------

.. nml:namelist:: JULES_TEMP_FIXES

.. nml:member:: ctile_orog_fix

   :type: integer
   :permitted: 0-2
   :default: 2

   If nonzero, corrects the surface exchange calculations in coastally
   tiled grid-boxes, assuming that the lowest level is physically
   terrain following and adjusting the temperature of the land/sea
   portions in accordance with their relative offset from the grid-box
   mean height using a dry/moist lapse rate where appropriate. Option 2
   will only adjust values over the sea.

.. nml:member:: i_fix_neg_snow

   :type: integer
   :permitted: 0, 1, 2, 3, 4
   :default: 2

   This replaces l_fix_neg_snow. Fixing issues related to the generation
   of negative amounts of snow has exposed a number of issues with the
   treatment of snow, and because fixing these issues has been a protracted
   process, we need to support a number of partial fixes for different
   defined applications. This requires that we replace the simple logical
   l_fix_neg_snow with an integer.

   The issues are discussed before the permitted options. Initial investigation
   of the causes of negative amounts of snow revealed that the original
   formulation of melting was incorrect (the erroneous use of the latent heat
   of condensation in the variable lcmelt), and that the interception of
   snow by an overloaded canopy could generate excessive melting leading
   to negative amounts of snow. Fixing these issues revealed problems with
   numerical rounding and underflows that required the reformulation of
   mass balance in terms of actual increments, rather than as a rate of
   melting or sublimation multipiled by the timestep, leading to a loss
   of bit-reproducibility. These fixes were introduced at version 7.5
   (Ticket #1396) under the logical l_fix_neg_snow, with the option .false.
   intended to result in no changes to the algorithm apart from the change
   from rates to increments, and the option .true. intended to apply the
   corrections.

   Unfortunately, a new bug was inadvertently introduced into the
   calculation of melting during the reformulation in terms of increments,
   and testing on a wider range of compilers after the release exposed
   another numerical problem with very small amounts of snow, leading to
   division by 0. Fixing these issues further exposed a problem with the
   formulation of the sublimation of snow from canopies when the canopy
   snow scheme was enabled, such that the presence of any positive amount
   of snow on a canopy would turn off evaporation of water, leading to an
   unnatural and unphysical sensitivity of the model to tiny amounts of
   snow. A minimal fix for this has been made available.

   The use of different versions of the fix in different configurations
   means that we are obliged to support the following options:

   0. Code equivalent to l_fix_neg_snow = .false., namely, the original
      code, except for the reformulation in terms of increments and the
      associated bug thereby introduced into melting.

   1. A version that corrects only the bug introduced under option 0,
      while retaining the original issues leading to the generation of
      negative amounts of snow. This is provided only for historical
      convenience for people upgrading from older version of JULES.

   2. The option that was previously available with l_fix_neg_snow = .true.
      This is currently the default in standalone JULES.

   3. A fix to the underlying problems and the bug in the calculation of
      melting, but not to the issue of the suppression of canopy
      evaporation by small amounts of snow. This option is required for the
      development of GC6 because it has been used in the development of
      this configuration.

   4. As option 3, but with a minimal fix to the unnatural sensitivity
      of evaporation to tiny amounts of snow.

.. nml:member:: l_accurate_rho

   :type: logical
   :default: F

   This switch improves the calculation of surface air density in the
   surface turbulent fluxes.  It includes appropriate use of dry air density
   when the atmospheric water vapour is expressed as a mixing ratio
   (l_mr_physics = .TRUE.), otherwise use the wet air density when
   it is expressed as a specific humidity.


.. nml:member:: l_dtcanfix

   :type: logical
   :default: F

   This switch corrects a bug in the evolution of the skin temperature in
   the implicit solver,
   whereby the change in the skin temperature is
   artificially constrained. This generally has a small effect,
   but can
   cause unphysical results if a canopy with a large heat capacity is
   coupled to an underlying substrate with a small heat capacity.

.. nml:member:: l_fix_alb_ice_thick

   :type: logical
   :default: F

   When zero-layer sea ice is used the thermodynamics is calculated in the
   UM through an effective thickness calculated from snow and ice thicknesses
   and associated thermal conductivities. With multi-layer sea ice the
   thermodynamics is calculated in the sea ice component of the model, and
   the effective thickness is no longer required.  However, it was still
   being used erroneously. This fix removes the effective thickness
   adjustment when multi-layer sea ice is used.

.. nml:member:: l_fix_albsnow_ts

   :type: logical
   :default: F

   The original version of the two-stream scheme to calculate the albedo
   of snow in JULES contains a bug in the calculation of the reflection
   coefficient that renders very thin layers of snow too reflective.
   This logical applies the appropriate correction when it is enabled.

.. nml:member:: l_fix_lake_ice_temperatures

   :type: logical
   :default: F

   If true, allows sea ice temperatures in lakes to evolve over time
   for coupled models when the lake is defined as a sea point but is
   not coupled to an ocean model.

.. nml:member:: l_fix_moruses_roof_rad_coupling

   :type: logical
   :default: F

   If true, this switch corrects a bug in the surface energy balance
   when the MORUSES radiative roof coupling is used
   (see :nml:mem:`JULES_URBAN::l_moruses_storage`).
   If false, the thermal conductivity of the soil (hcons) is erroneously
   set to zero, which causes the roof to be effectively uncoupled when
   :nml:mem:`JULES_VEGETATION::l_vegcan_soilfx`.

.. nml:member:: l_fix_neg_snow

   :type: logical
   :default: F

   This has been replaced by :nml:mem:`JULES_TEMP_FIXES::i_fix_neg_snow` .

.. nml:member:: l_fix_osa_chloro

   :type: logical
   :default: F

   When set to false, the chlorophyll content used to determine the optical
   properties of water, for the ocean surface albedo, are specified in gm-3
   when the parameterisation they use is defined in mg m-3.
   It is a short term logical until the code becomes the new default.

.. nml:member:: l_fix_snow_frac

   :type: logical
   :default: F

   When set to  false, there is the potential to have small snow mass, but a
   zero snow fraction due to machine precision in the calculations. This
   prevents sublimation or snow melt from removing the remaining snow mass,
   hence small values can persist.
   In addition to this there is a conceptual bug in the calculation of
   the fraction of potential evaporation because it does not add in canopy
   evaporation when the snow fraction is less than one.
   When set to true these issues are corrected and in addition the radiation
   calculations for snow fraction are also made consistent.

.. nml:member:: l_fix_ustar_dust

   :type: logical
   :default: F

   If true, corrects how ustar is calculated in the exchange
   coefficient for dust deposition

.. nml:member:: l_fix_wind_snow

   :type: logical
   :default: F

   If true, ensures that wind speed is calculated for use in snow unloading.
   If false, the wind speed for unloading will be zero on timesteps when
   10m wind diagnostics are not calculated. This will tend to leave more
   snow on the vegetation.
   It is a short term logical until the code becomes the new default.

.. nml:group:: For UM-coupled JULES applications, the following switches are set during the run to be equivalent to the corresponding switches in the UKCA (which are set in the UM run_ukca namelist).

  .. nml:member:: l_fix_drydep_so2_water

     :type: logical
     :default: FALSE

     Switch in UKCA to correct the surface resistance of SO2 to water when calculating
     the dry deposition of SO2. If true, a surface resistance value of 10 s m-1
     is used. If false, an incorrect surface resistance value of 148.9 s m-1 is used.
     Note: this is being maintained as it was used in UKESM1.0.

  .. nml:member:: l_fix_improve_drydep

     :type: logical
     :default: FALSE

     Switch in UKCA to make the surface resistance terms for the 9-tile configuration
     consistent with those of the 13, 17 and 27 tile configurations. If true, use
     surface resistance terms for the 9-tile configuration that are consistent
     with those for the other allowed surface tile configurations. If false, use
     the original surface resistance terms for the 9-tile configuration.

     The switch also adjusts the surface resistance terms and hence dry deposition velocities
     for the following chemical species: HCl, HOCl, HBr, HOBr, H2SO4, MeOH and Sec_Org.

  .. nml:member:: l_fix_ukca_h2dd_x

     :type: logical
     :default: FALSE

     Switch in UKCA to correct a bug in the elements of h2dd_c and h2dd_m used for the calculation of
     the H2 dry deposition to shrub and bare soil. If true, use the corrected elements of
     h2dd_c and h2dd_m. If false, use the original elements of h2dd_c and h2dd_m.

