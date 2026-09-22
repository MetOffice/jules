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

MODULE imogen_check_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE imogen_check(                                                       &
  c_emissions, include_co2, include_non_co2_radf, land_feed_co2,               &
  land_feed_ch4, ocean_feed, change_metdata_method, l_change_metdata)

USE logging_mod, ONLY: log_fatal, log_warn

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This code is designed to make a quick cross-check to ensure that
!   the flags in IMOGEN are set properly for the currently allowed configu
!   The currently allowed configurations should fit with the available doc
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
! Written by: C. Huntingford (4th March 2004) then updated by Edward Comyn-Platt
! and Eleanor Burke to include the checks in imogen_confirmed_run.F90 and remove
! any duplication.
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

LOGICAL, INTENT(IN) ::                                                         &
  c_emissions,                                                                 &
         ! True: CO2 concentration is calculated from emissions read from
         !         file_scen_emits
         ! False: CO2 concentration read from file_scen_co2_ppmv
  include_co2,                                                                 &
         ! Are adjustments to CO2 concentrations allowed?
  include_non_co2_radf,                                                        &
         ! Are adjustments to non-CO2 radiative forcings allowed?
  land_feed_co2,                                                               &
         ! Are land CO2 feedbacks allowed on atmospheric C
  land_feed_ch4,                                                               &
         ! Are land CH4 feedbacks allowed on atmospheric C
  ocean_feed,                                                                  &
         ! Are ocean feedbacks allowed on atmospheric C
  l_change_metdata
         ! True if the meteorological driving data changes over time

INTEGER, INTENT(IN) ::                                                         &
  change_metdata_method
         ! 1: analogue model
         ! 2: inputed anomalies (need co2 concs)
         !    no feedbacks included
         ! 3: drive with global mean temperature changes (need co2 concs)
         !    no feedbacks currently included

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  check_flag,                                                                  &
         ! Checks that configuration is valid
  confirm_flag
         ! Confirms that configuration has been used before

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'imogen_check'

check_flag   = .FALSE.
confirm_flag = .FALSE.

! initialise imogen_check message with line of stars
WRITE(jules_message,*) '***********************************************'
CALL jules_print(RoutineName, jules_message)

! Now make checks on structures which will hopefully all be available at
! To make this code easy to read, simple "if"
! statements comments are used for each instance and case printed.

IF ( l_change_metdata ) THEN
  ! either prescribed anomalies or analogue model or drive with gmt
  check_flag = .TRUE.
  WRITE(jules_message,*) 'l_change_metdata=True'
  CALL jules_print(RoutineName, jules_message)
  WRITE(jules_message,*) 'Run uses changing meteorological driving data'
  CALL jules_print(RoutineName, jules_message)
  IF ( change_metdata_method  == 1) THEN
    WRITE(jules_message,*) 'Run is using meterological changes derived from pattern '
    CALL jules_print(RoutineName, jules_message)
    WRITE(jules_message,*) 'scaling with global temperature changing using '
    CALL jules_print(RoutineName, jules_message)
    WRITE(jules_message,*) 'the intermediate climate model'
    CALL jules_print(RoutineName, jules_message)
  ELSE IF ( change_metdata_method  == 2) THEN
    WRITE(jules_message,*) 'Run is using user prescribed meterological anomalies'
    CALL jules_print(RoutineName, jules_message)
  ELSE IF ( change_metdata_method  == 3) THEN
    WRITE(jules_message,*) 'Run uses prescribed global mean temperature changes'
    CALL jules_print(RoutineName, jules_message)
    WRITE(jules_message,*) 'there are currently no land or ocean feedbacks'
    CALL jules_print(RoutineName, jules_message)
  END IF
END IF

IF ( .NOT. l_change_metdata) THEN
  check_flag   = .TRUE.
  WRITE(jules_message,*) 'Run uses input climatological met data, '
  CALL jules_print(RoutineName, jules_message)
  WRITE(jules_message,*) 'no changes in climate, l_change_metdata=False'
  CALL jules_print(RoutineName, jules_message)
  WRITE(jules_message,*)                                                       &
    'If spin-up, check settings for subsequent transient run'
  CALL jules_print(RoutineName, jules_message)

  IF (c_emissions) THEN
    WRITE(jules_message,*) 'User provided Anthro. CO2 emissions'
    CALL jules_print(RoutineName, jules_message)
  ELSE ! no anthropogenic co2 emissions
    WRITE(jules_message,*) 'No Anthropogenic CO2 emissions provided'
    CALL jules_print(RoutineName, jules_message)
    IF (include_co2) THEN
      WRITE(jules_message,*) 'Allows user to provide a file of CO2 concs.'
    ELSE ! not include_co2 and not c_emissions
      WRITE(jules_message,*) 'Simulation with fixed CO2 concentration'
    END IF
    CALL jules_print(RoutineName, jules_message)
  END IF

  ! Confirmed configurations (i.e. people have used this configuration before)
  IF (( .NOT. c_emissions) .AND. ( .NOT. land_feed_ch4) .AND.                  &
      ( .NOT. include_non_co2_radf) .AND. ( .NOT. land_feed_co2) .AND.         &
      ( .NOT. ocean_feed))                                                     &
      THEN !include_co2 is either true or false
    confirm_flag = .TRUE.
  END IF
END IF

! Currently coded analogue model possibilities
IF (l_change_metdata .AND. change_metdata_method == 1) THEN
  WRITE(jules_message,*) 'Analogue model simulations'
  CALL jules_print(RoutineName, jules_message)

  IF (( .NOT. include_co2) .AND. include_non_co2_radf) THEN
    WRITE(jules_message,*) 'Run is for non-CO2 gases only'
    CALL jules_print(RoutineName, jules_message)
    check_flag = .TRUE.
  END IF

  IF (include_co2) THEN
    ! All configurations with co2 are technically valid
    check_flag = .TRUE.

    IF ( .NOT. c_emissions) THEN
      ! Write out CO2 concentrations statement
      WRITE(jules_message,*) 'Run is for prescribed CO2 concentration'
      CALL jules_print(RoutineName, jules_message)
      WRITE(jules_message,*) 'Allows user to provides a file of CO2 concs.'
      CALL jules_print(RoutineName, jules_message)

      ! Check for confirmed configurations:
      IF (( .NOT. land_feed_co2) .AND. ( .NOT. ocean_feed) .AND.               &
          ( .NOT. land_feed_ch4)) THEN
        confirm_flag = .TRUE.
      END IF

    ELSE ! if c_emissions true

      ! Write out CO2 emissions statement
      WRITE(jules_message,*) 'Carbon cycle driven by prescribed emissions'
      CALL jules_print(RoutineName, jules_message)
      WRITE(jules_message,*) 'Allows user to provides a file of CO2 emissions.'
      CALL jules_print(RoutineName, jules_message)

      ! Write which CO2 feedbacks are turned on
      IF (land_feed_co2) THEN
        WRITE(jules_message,*) 'There are land CO2 feedbacks'
      ELSE
        WRITE(jules_message,*) 'There are NO land CO2 feedbacks'
      END IF
      CALL jules_print(RoutineName, jules_message)

      IF (ocean_feed) THEN
        WRITE(jules_message,*) 'There are ocean CO2 feedbacks'
      ELSE
        WRITE(jules_message,*) 'There are NO ocean CO2 feedbacks'
      END IF
      CALL jules_print(RoutineName, jules_message)

      ! Check for confirmed configurations:
      IF ( land_feed_co2 .AND. ocean_feed .AND. ( .NOT. land_feed_ch4)) THEN
        confirm_flag = .TRUE.
      END IF

    END IF ! end c_emissions true
  END IF ! End include_co2 true

  ! Checks for non-co2 components
  IF (include_non_co2_radf) THEN
    WRITE(jules_message,*) 'Run is with prescribed non-CO2 radiative forcing'
    CALL jules_print(RoutineName, jules_message)
    WRITE(jules_message,*) 'User provides file of Non-CO2 radiative forcing'
    CALL jules_print(RoutineName, jules_message)

    ! Write if CH4 feedbacks are turned on
    IF (land_feed_ch4) THEN
      WRITE(jules_message,*) 'There are land CH4 feedbacks'
    ELSE
      WRITE(jules_message,*) 'There are NO land CH4 feedbacks'
    END IF
    CALL jules_print(RoutineName, jules_message)

    ! Check for confirmed configurations
    IF ( include_co2 .AND. ocean_feed .AND. ( land_feed_ch4)) THEN
      confirm_flag = .TRUE.
    END IF
  ELSE
    ! Failsafe for if CH4 feedbacks are on without non-CO2 Radiative Forcing
    IF (land_feed_ch4) THEN
      WRITE(jules_message,*) 'Land CH4 feedbacks require non_CO2 radiative'//  &
                             ' forcing to use as baseline'
      CALL jules_print(RoutineName, jules_message)
      check_flag = .FALSE.
    END IF
  END IF

END IF ! end if analogue true.

IF (l_change_metdata .AND. change_metdata_method == 3) THEN
  WRITE(jules_message,*) 'JULES is driven with global mean temperature changes'
  CALL jules_print(RoutineName, jules_message)
  WRITE(jules_message,*) 'there are no land or ocean feedbacks'
  CALL jules_print(RoutineName, jules_message)
  IF (land_feed_ch4) THEN
    WRITE(jules_message,*) 'Land CH4 feedbacks are not allowed'
    CALL jules_print(RoutineName, jules_message)
    check_flag = .FALSE.
  END IF
  IF (land_feed_co2) THEN
    WRITE(jules_message,*) 'Land CO2 feedbacks are not allowed'
    CALL jules_print(RoutineName, jules_message)
    check_flag = .FALSE.
  END IF
  IF (ocean_feed) THEN
    WRITE(jules_message,*) 'Ocean feedbacks are not allowed'
    CALL jules_print(RoutineName, jules_message)
    check_flag = .FALSE.
  END IF

  IF ( c_emissions ) THEN
    WRITE(jules_message,*) 'Need co2 concentration not emissions'
    !FAIL
    CALL jules_print(RoutineName, jules_message)
    check_flag = .FALSE.
  ELSE
    IF ( .NOT. include_co2) THEN
      WRITE(jules_message,*) 'Need time series of co2 conc read from file'
      CALL jules_print(RoutineName, jules_message)
      check_flag = .FALSE.
    ELSE
      check_flag = .TRUE.
    END IF
  END IF
END IF

IF ( .NOT. check_flag)                                                         &
  CALL log_fatal("IMOGEN_CHECK",                                               &
                 'Combination not yet allowed')

IF ( .NOT. confirm_flag)                                                       &
  CALL log_warn("IMOGEN_CHECK",                                                &
    'This combination of flags has not yet been tested, proceed with caution')

! Finilise imogen_check message with line of stars
WRITE(jules_message,*) '***********************************************'
CALL jules_print(RoutineName, jules_message)

RETURN

END SUBROUTINE imogen_check
END MODULE imogen_check_mod
#endif
