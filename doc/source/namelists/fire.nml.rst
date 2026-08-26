``fire.nml``
===================

This file contains two namelists one called :nml:lst:`JULES_FIRE_WEATHER_INDEX` that sets time-invariant parameters for performing wildfire-related calculations. The second namelist is for parameters associated with the inferno fire model called :nml:lst:`JULES_INFERNO`

``FIRE_SWITCHES`` namelist members
-----------------------------------

.. nml:namelist:: FIRE_SWITCHES

.. nml:member:: l_fire

   :type: logical
   :default: F

   Switch to enable the fire module.

   TRUE
       The fire module will be executed according to the settings of subsequent namelist members.

   FALSE
       The fire module will not be executed and subsequent members of the namelist will have no effect.


.. nml:member:: mcarthur_flag

   :type: boolean
   :default: F

   Switch for calculating the McArthur Forest Fire Danger Index (FFDI).


.. nml:member:: mcarthur_opt

   :type: real
   :default: MDI

   Switch for choosing which method of calculating the soil moisture deficit required for the McArthur Forest Fire Danger Index (FFDI). 1 uses the model soil moisture, 2 uses a fixed value of 120 mm.

.. nml:member:: canadian_flag

   :type: boolean
   :default: F

   Switch for calculating the Canadian Fire Weather Index (FWI).

.. nml:member:: canadian_hemi_opt

   :type: boolean
   :default: F

   If TRUE, then the month-dependent parameters used in the calculation will be offset by 6 months for the southern hemisphere. This will cause a discontinuity in results when crossing the equator.

.. nml:member:: nesterov_flag

   :type: boolean
   :default: F

   Switch for calculating the Nesterov Index.


``JULES_INFERNO`` namelist members
-----------------------------------

.. nml:namelist:: JULES_INFERNO

   .. nml:member:: l_inferno

   :type: boolean
   :default: F

   Switch that determines whether interactive fires (INFERNO) is
   used. This allows for the diagnostic of burnt area, burnt carbon
   and a variety of fire emissions.

   TRUE
       INFERNO is used to provide diagnostic fire variables

   FALSE
       INFERNO is not used.

.. nml:member:: ignition_method

   :type: integer
   :permitted: 1, 2, 3
   :default: 1

   Switch to determine the type of ignition used (ubiquitous or prescribed with population and lightning)

   1.  INFERNO uses ubiquitous (constant) ignitions, of 1.67 fires km\
       :sup:`-2` s\ :sup:`-1` (1.5 from humans, 0.17 from lightning).

   2.  INFERNO uses prescribed lightning ignitions, either from an ancillary or the UM.
       Meanwhile humans are assumed to ignite 1.5 fires km\ :sup:`-2` s\ :sup:`-1`.

   3.  INFERNO uses prescribed ignition using Population Density and Lightning Frequency (Cloud-to-Ground).
       These must be provided as prescribed data to the JULES run.

.. nml:member:: l_trif_fire

   :type: boolean
   :default: F

   Switch that determines whether interactive fire is used. This allows for burnt area to link with dynamic
   vegetation.

   Only used if :nml:mem:`JULES_VEGETATION::l_triffid` = TRUE.

   TRUE
       Burnt area is calculated in INFERNO and passed to TRIFFID to
       calculate vegetation dynamics. Carbon is also removed from DPM
       and RPM pools in SOILCARB.
   FALSE
       Burnt area is zero unless prescribed via an ancillary file.

   .. nml:member:: z_burn_max

      :type: real
      :default: 0.2

      Parameter controlling the depth to which fire burns soil litter carbon in metres. At depths shallower than this value, the fire can burn soil carbon in the two litter pools (dpm and rpm). If z_burn_max falls within a layer only a proportion of the soil carbon is burnt. Only used with layered soil carbon scheme (:nml:mem:`JULES_SOIL_BIOGEOCHEM::l_layeredc` = TRUE) and fire (either :nml:mem:`JULES_INFERNO::l_trif_fire` or :nml:mem:`JULES_INFERNO::l_inferno` or both). In reality the burn depth varies so please check whether the default value of 0.2 is suitable for your application.

   .. nml:member:: flam_sm_func

      :type: integer
      :permitted: 1, 2
      :default: 1

      Parameter controlling the relationship between soil moisture and flammability.
      1 a linear relatonship which has a flammability of 0 at 1 and 1 at 0.
      2 an exponential relationship controlled by :nml:mem:`flam_sm_low` and :nml:mem:`flam_sm_up`

   .. nml:member:: flam_sm_low

      :type: real
      :default: 0.0

   Only used if :nml:mem:`flam_sm_func` = 2.

   .. nml:member:: flam_sm_up

      :type: real
      :default: 2.4

   Only used if :nml:mem:`flam_sm_func` = 2.

   .. nml:member:: flam_rhum_low
 
      :type: real
      :default: 0.1

   .. nml:member:: flam_rhum_up

      :type: real
      :default: 0.9

   .. nml:member:: flam_rain_const

      :type: real
      :default: -14929920000.0


buggy

   .. nml:member:: flam_fuel_low

      :type: real
      :default: 0.02

   .. nml:member:: flam_fuel_up
      :type: real
      :default: 0.2

   .. nml:member:: triffire_ccdpm_min

      :type: real
      :default: 0.8

    Minimum DPM soil carbon pool combustion completness fraction.

   .. nml:member:: triffire_ccdpm_max

      :type: real
      :default: 1.0

    Maximum DPM soil carbon pool combustion completness fraction.

   .. nml:member:: triffire_ccrpm_min

      :type: real
      :default: 0.0

    Minimum RPM soil carbon pool combustion completness fraction.


   .. nml:member:: triffire_ccrpm_max

      :type: real
      :default: 0.2

    Maximum RPM soil carbon pool combustion completness fraction.
