JULES version 8.3 Release Notes
===============================

The JULES vn8.3 release consists of approximately 19 contributions, including work by many people. The `JULES repository <https://github.com/MetOffice/jules>`_ is fully open to public access following adoption of the BSD 3-Clause licence.

Full details of the changes committed for JULES vn8.3 can be found on the `JULES GitHub repository <https://github.com/MetOffice/jules/milestone/4>`_.

Issue or pull request numbers are indicated below, e.g. #32.

Science changes
---------------

 * Added representations of reservoir hydrology (see :nml:mem:`JULES_RIVERS::l_reservoirs` and :nml:mem:`JULES_RIVERS::l_minor_reservoirs`). (#32, 148)
 * Allowed the irrigated and non-irrigated bare soil evaporation terms to be calculated from the irrigated and non-irrigated soil moisture columns rather than the grid-box mean soil moisture as previously (see :nml:mem:`JULES_IRRIG::l_soil_evap_irrig_separate`). (#139)
 * Added a zenith angle dependence to the calculation of albedo for bare sea ice, snow on sea ice and melt ponds (see `l_zenith_albedo` in the `JULES_SEA_SEAICE` namelist). (#88)
 * Allowed use of the existing vegetation phenology model with the RED/veg3 demography scheme. (#90)
 * Soil heating from biogeochemical processes (see :nml:mem:`JULES_SOIL_BIOGEOCHEM::l_bgc_heat`) can be now considered with the one-layer soil carbon model and when used with the UM atmosphere model. (#153)


Bug fixes
---------

 * Further modifications to deal with negative snow amounts. The previous switch `l_fix_neg_snow` has been replaced by :nml:mem:`JULES_TEMP_FIXES::i_fix_neg_snow`. The default value of 2 recreates the behaviour previously given by `l_fix_neg_snow` = .TRUE.. (#167)
 * Fixed the JULES upgrade macro for rivers-standalone compatability. (#173)
 * The land fraction used with the rivers code can be flipped in latitude to retain consistency with other fields. (#144)


General/Technical changes
-------------------------

 * Fire parameters separated into :nml:lst:`JULES_FIRE_WEATHER_INDEX` which contains switches used to calculate the different fire weather indices, and :nml:lst:`JULES_INFERNO` for parameters associated with the INFERNO fire model. (#61, 141, 147)
 * Accounted for previously missing crop surface types. (#136)
 * Wrapped further "bare" subroutines in modules. (#135)
 * Migrated remaining JULES metadata from the UM repository to jules-um and jules-shared. (#154)
 * Science module leaders included in CODEOWNERS file. (#146)
 * Updated GitHub Actions workflows. (#133)


Changes to testing
------------------

 *  Rose stem tests using IMOGEN at the Met Office changed to not use OMP, and a fix related to a change in the scheduler. (#161, 177)


Documentation and licence updates
---------------------------------

 *  Updates associated with many of the above changes, release notes, and more. (#181)

Documentation can be viewed on the github page `<https://metoffice.github.io/jules/>`_.
