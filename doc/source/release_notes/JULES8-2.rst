JULES version 8.2 Release Notes
===============================

The JULES vn8.2 release consists of approximately 22 contributions, including work by many people. The `JULES repository <https://github.com/MetOffice/jules>`_ is now fully open to public access following adoption of the BSD 3-Clause licence.

Full details of the changes committed for JULES vn8.2 can be found on the `JULES GitHub repository <https://github.com/MetOffice/jules/milestone/3>`_.

Issue numbers are indicated below, e.g. #72.

Science changes
---------------

 * The form of the relationship between soil decomposition and soil moisture is controlled by :nml:mem:`JULES_SOIL_BIOGEOCHEM::cs_decomp_soil_moist_func`, allowing for reduced decomposition in very wet soils. (#72)
 * Heating from decomposition of soil is included via switch :nml:mem:`JULES_SOIL_BIOGEOCHEM::l_bgc_heat`. (#111)
 * A new irrigation scheme, activated by the switch :nml:mem:`JULES_IRRIG::irrig_option`, applies new methods for irrigation on the surface tiles. The code now has two additional grass tiles (C3_irrig and C4_irrig) which are irrigated (see :nml:mem:`JULES_PFTPARM::irrig_pft_io`). On these tiles, the soil moisture availability factor is set to 1.0 and soil moisture extraction is disabled. This functionality can only be used with :nml:mem:`JULES_IRRIG::l_irrig_dmd` = FALSE. (#107)


Bug fixes
---------
 *  Fixed a bug (which previously would result in a segmentation fault) related to metstats when running JULES standalone with :nml:mem:`FIRE_SWITCHES::l_fire` = TRUE and more than one and point. (#85)
 *  Fixed issues in the daily weather disaggregator (see :nml:mem:`JULES_DRIVE::l_daily_disagg`) which previously would invert the diurnal cycle in high latitude regions under polar day/night. This will change existing results. (#83)


General/Technical changes
-------------------------

 *  Inland river flow (for endorheic basics) is passed through the OASIS coupler, for coupling to an ocean model. This is enabled via :nml:mem:`JULES_HYDROLOGY::l_inland` and only affects the rivers-only build of JULES (:nml:mem:`JULES_MODEL_ENVIRONMENT::lsm_id` = 3). (#89)
 *  Metadata for :nml:lst:`JULES_PFTPARM` migrated to the shared metadata in jules-shared. (#42)
 *  Added the jules-lfric metadata section (moved from lfric_apps). (#110)
 *  Corrections to workflow. (#71, 94)
 *  Added a CODEOWNERS file to the JULES repository to notify code owners when their areas of the JULES source code are updated, and later updates to that file (#87, 109, 112)


Changes to testing
------------------

 *  Replaced the Rose Stem configuration for NIWA's previous HPC with a new ESNZ Cascade configuration. It also removes tests for which input files are not currently publicly available and introduces a compiler configuration for Intel OneAPI compilers. (#108)


Documentation and licence updates
---------------------------------

 *  Updates associated with many of the above changes, release notes, and more. (#45, 80, 124)
 *  Addition of the JULES Contributor Licence Agreement. (#50)

Documentation can be viewed on the github page `<https://metoffice.github.io/jules/>`_.
