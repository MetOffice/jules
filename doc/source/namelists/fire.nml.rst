``fire.nml``
===================

This file contains two namelists one called :nml:lst:`JULES_FIRE_WEATHER_INDEX` that contains switches used to calculate the different fire weather indices available. The second namelist is for parameters associated with the inferno fire model called :nml:lst:`JULES_INFERNO`

``JULES_FIRE_WEATHER_INDEX`` namelist members
-----------------------------------

.. nml:namelist:: JULES_FIRE_WEATHER_INDEX

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

.. nml:member:: z_burn_max

   :type: real
   :default: 0.2

   Parameter controlling the depth to which fire burns soil litter carbon in metres. At depths shallower than this value, the fire can burn soil carbon in the two litter pools (dpm and rpm). If z_burn_max falls within a layer only a proportion of the soil carbon is burnt. Only used with layered soil carbon scheme (:nml:mem:`JULES_SOIL_BIOGEOCHEM::l_layeredc` = TRUE) and fire (either :nml:mem:`JULES_INFERNO::l_trif_fire` or :nml:mem:`JULES_INFERNO::l_inferno` or both). In reality the burn depth varies so please check whether the default value of 0.2 is suitable for your application.

.. nml:member:: flam_sm_func

   :type: integer
   :permitted: 1, 2
   :default: 1

   Switch used to define the function used to parameterise the relationship between soil moisture and flammability

   1.  A linear relationship which has a flammability of 0 at saturation and 1 when soil is completely dry.

   2.  An exponential relationship controlled by :nml:mem:`flam_sm_low` and :nml:mem:`flam_sm_up`

   .. nml:member:: flam_sm_low

   :type: real
   :default: 0.0

   Soil moisture below which flammability is 1.0. Expressed as a fraction of saturation. Only used if :nml:mem:`flam_sm_func` = 2.

.. nml:member:: flam_sm_up

   :type: real
   :default: 2.4

   Exponential decay parameter for relationship between soil moisture and flammability. Only used if :nml:mem:`flam_sm_func` = 2.

.. nml:member:: flam_rhum_low
 
   :type: real
   :default: 10.0

   Lower relative humidity threshold for relationship between relative humidity and flammability below which value of function is 1.0. Expressed as a percentage.

.. nml:member:: flam_rhum_up

   :type: real
   :default: 90.0

   Upper relative humidity threshold for relationship between relative humidity and flammability above which value of function is 0.0. Expressed as a percentage.

.. nml:member:: flam_rain_const

   :type: real
   :default: 14929920000.0

   An exponential decay factor that defines the relationship between flammability and rainfall.
   In order to recreate the relationship hardwired into JULES version 8.2 and lower this value 
    should be set to 14929920000.0. However, this is not the relationship that
   was documented by Mangeon et al. (2016). If you want the Mangeon et al. relationship 
    then this value should be 172800.0.
   If you want to remove the dependence of flammability on rainfall then this value should be set to 0.0.

   .. seealso::
      References:

      * Mangeon, S., Voulgarakis, A., Gilham, R., Harper, A., Sitch, S., and Folberth, G.: INFERNO: a fire and emissions scheme for the UK Met Office’s Unified Model, Geosci. Model Dev., 9, 2685-2700, https://doi.org/10.5194/gmd-9-2685-2016, 2016.

.. nml:member:: flam_fuel_low

   :type: real
   :default: 0.02

.. nml:member:: flam_fuel_up
   :type: real
   :default: 0.2

.. nml:member:: ccdpm_min

   :type: real
   :default: 0.8

   Minimum DPM soil carbon pool combustion completeness fraction.

.. nml:member:: ccdpm_max

   :type: real
   :default: 1.0

   Maximum DPM soil carbon pool combustion completeness fraction.

.. nml:member:: ccrpm_min

   :type: real
   :default: 0.0

   Minimum RPM soil carbon pool combustion completeness fraction.


.. nml:member:: ccrpm_max

   :type: real
   :default: 0.2

   Maximum RPM soil carbon pool combustion completeness fraction.
