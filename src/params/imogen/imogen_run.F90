#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE imogen_run

USE io_constants, ONLY: max_file_name_len

USE missing_data_mod, ONLY: imdi, rmdi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Default parameters and variables required for the general imogen setup
!     Values can be set in the imogen.nml
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

REAL ::                                                                        &
  co2_init_ppmv = rmdi,                                                        &
              ! Initial CO2 concentration (ppmv)
  ch4_init_ppbv = rmdi,                                                        &
              ! Initial CH4 concentration (ppbv)
  fch4_ref = rmdi,                                                             &
              ! Reference wetland CH4 emissions (Tg CH4/yr)
  tau_ch4_ref = rmdi,                                                          &
              ! Lifetime of CH4 in atmosphere yr_fch4_ref (years).
              ! Value used in Gedney et al. (2004) S3 (Table 1)
              ! from TAR, Table 4.3 (subscript d)
  ch4_ppbv_ref = rmdi
              ! Reference atmosphere CH4 concentration at yr_fch4_ref (ppbv)


CHARACTER(LEN=max_file_name_len) ::                                            &
  file_scen_emits = '',                                                        &
              ! If used, file containing CO2 emissions in Gt C
  file_non_co2_radf = '',                                                      &
              ! If required, file containing non-CO2 radiative forcing
  file_scen_co2_ppmv = '',                                                     &
              ! If used, file containing CO2 atm concentrations
  file_ch4_n2o = ''
              ! If used, file contain CH4 and N2O atmos concs, required if
              ! land_feed_ch4=True


LOGICAL ::                                                                     &
  l_change_metdata = .TRUE.,                                                   &
              ! If true, then allow the driving data to change over time
  l_daily_metdata_climatol = .FALSE.,                                          &
              ! Flag showng whether met climatological is daily or monthly
  c_emissions = .TRUE.,                                                        &
              ! If true, means CO2 concentration is calculated from emissions
  include_co2 = .TRUE.,                                                        &
              ! Are adjustments to CO2 values allowed?
  include_non_co2_radf = .TRUE.,                                               &
              ! Are adjustments to radiative forcing from non-CO2 allowed?
  land_feed_co2 = .FALSE.,                                                     &
              ! Are land CO2 feedbacks allowed on atmospheric CO2 conc.
  land_feed_ch4 = .FALSE.,                                                     &
              ! Are land CH4 feedbacks allowed on atmospheric CH4 conc.
  ocean_feed = .FALSE.
              ! Are ocean feedbacks allowed on atmospheric CO2 concentration

INTEGER ::                                                                     &
  change_metdata_method = imdi,                                                &
      ! Switch for the calculation method of l_change_metdata
      !   change_metdata_method=1: analogue model
      !   change_metdata_method=2: anomalies
      !   change_metdata_method=3: drive with global mean temperatures
              ! Drive with annual mean global temperatures
              ! This takes the patterns and makes the driving data by
              ! combining the global temperatures and the patterns
              ! also need CO2 concentrations as an input to JULES
              ! (c_emissions=.FALSE.) There are no feedbacks.
              ! (land_feed_co2=.FALSE.,land_feed_ch4=.FALSE.,ocean_feed=.FALSE.)
              ! include_non_co2_radf=.FALSE., include_co2=.FALSE.,
              ! l_change_metdata=.TRUE.,change_metdata_method=3
              ! need a filename for the time series of global mean temperatures
              ! and for the co2 concentrations.
  nyr_non_co2 = imdi,                                                          &
            ! Number of years for which NON_CO2 forcing is prescribed.
  nyr_emiss = imdi,                                                            &
              ! Number of years of emission data in file.
  initial_co2_ch4_year = imdi,                                                 &
              ! Year of initialisation CO2/CH4 value: required to get
              ! ocean feedback correct on restart
  nyr_ch4_n2o = imdi,                                                          &
              ! Number of years of CH4 & N2O data in file.
  yr_fch4_ref = imdi
              ! Year for reference wetland CH4 emissions and atmospheric
              ! CH4 decay rate, i.e. fch4_ref, tau_ch4_ref & ch4_ppbv_ref

LOGICAL :: initialise_from_dump = .FALSE.
              ! T - initialise variables from a dump file
              ! F - let IMOGEN handle initialisation
CHARACTER(LEN=max_file_name_len) :: dump_file
              ! The dump file to initialise from if required

NAMELIST  / imogen_run_list / co2_init_ppmv, ch4_init_ppbv, file_scen_emits,   &
                           file_scen_co2_ppmv,nyr_emiss,                       &
                           c_emissions, include_co2,                           &
                           include_non_co2_radf, land_feed_co2, land_feed_ch4, &
                           ocean_feed, nyr_non_co2,                            &
                           l_change_metdata, change_metdata_method,            &
                           l_daily_metdata_climatol,                           &
                           file_non_co2_radf, file_ch4_n2o,                    &
                           initialise_from_dump, dump_file,                    &
                           initial_co2_ch4_year, yr_fch4_ref, nyr_ch4_n2o,     &
                           fch4_ref, tau_ch4_ref, ch4_ppbv_ref



END MODULE imogen_run
#endif
