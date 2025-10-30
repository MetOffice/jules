#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE register_output_profile(profile_name, output_initial,               &
                                   output_spinup, output_main_run,             &
                                   l_land_frac, output_start, output_end,      &
                                   output_period, sample_period, file_period,  &
                                   identifiers, var_names, output_types)

USE datetime_mod, ONLY: period_day, period_month, period_year, secs_in_day,    &
                        datetime_to_string

USE model_time_mod, ONLY: max_spinup_cycles, main_run_start, main_run_end,     &
                           timestep_len

USE model_interface_mod, ONLY: get_var_id, is_river_var

USE jules_rivers_mod, ONLY: l_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Register an output profile to output given variables over the given
!   period
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: profile_name
    ! The name of this profile - only used in filenames

LOGICAL, INTENT(IN) :: output_initial
    ! T - this profile should output initial data
    ! F - this profile should not output initial data

LOGICAL, INTENT(IN) :: output_spinup
    ! T - profile will provide output during spinup
    ! F - profile will not provide output during spinup

LOGICAL, INTENT(IN) :: output_main_run
    ! T - profile will provide output for the specified portion of the
    !     main run only
    ! F - profile will not provide any output during the main run

LOGICAL, INTENT(IN) :: l_land_frac
    ! T - Gridbox land fraction will be output

TYPE(datetime), INTENT(IN) :: output_start, output_end
    ! USED ONLY IF output_main_run=T
    ! Start and end dates for output if providing output for main run

INTEGER, INTENT(IN) ::                                                         &
  output_period,                                                               &
    ! The output period for this profile - this should be a multiple of
    ! the model timestep or a 'special' period (monthly/yearly)
  sample_period,                                                               &
    ! The sampling period for this profile (s). This should be a multiple of
    ! the model timestep length and a factor of output_period.
  file_period
    ! This should be a special period for monthly or yearly files
    ! Any other value will result in a single file for the whole of the
    ! output period

CHARACTER(LEN=*), INTENT(IN) :: identifiers(:)
    ! The model identifiers of the variables this profile provides output for

CHARACTER(LEN=*), INTENT(IN) :: var_names(:)
    ! The name to use in output files for each variable
    ! Each value can be the empty string - if it is, the model identifier will
    ! be used

CHARACTER(LEN=*), INTENT(IN) :: output_types(:)
    ! The type of output to use for each variable


! Work variables
INTEGER :: nvars  ! The number of output fields in the profile
INTEGER :: test_length  ! A length of time used in testing (s).
LOGICAL :: have_river_variable ! Flag indicating that a river point
                               ! variable has been detected.

! Local versions of INTENT(IN) arguments that we can modify
LOGICAL :: output_spinup_local
TYPE(datetime) :: output_start_local, output_end_local
CHARACTER(LEN=LEN(var_names)) :: var_names_local(SIZE(var_names))

INTEGER :: i  ! Loop counters


!-----------------------------------------------------------------------------

! Check that we have space to register another profile
IF ( nprofiles >= nprofiles_max )                                              &
  CALL log_fatal("register_output_profile",                                    &
                 "Too many profiles registered - try increasing nprofiles_max")

! Check that we have an output type for each variable
nvars = SIZE(identifiers)

IF ( nvars /= SIZE(var_names) )                                                &
  CALL log_fatal("register_output_profile",                                    &
                 "identifiers and var_names must have the same " //            &
                 "number of elements")

IF ( nvars /= SIZE(output_types) )                                             &
  CALL log_fatal("register_output_profile",                                    &
                 "identifiers and output_types must have the same " //         &
                 "number of elements")

!-----------------------------------------------------------------------------
! Check that arguments make sense
!-----------------------------------------------------------------------------
! First copy INTENT(IN) arguments that we might want to modify into local
! versions
output_spinup_local = output_spinup
output_start_local  = output_start
output_end_local    = output_end

! Check that we have been given a profile name
IF ( LEN_TRIM(profile_name) == 0 )                                             &
  CALL log_fatal("register_output_profile", "No profile name given")

! Check that this profile name has not already been used.
IF ( ANY( profiles(1:nprofiles)%profile_name == profile_name ) ) THEN
  CALL log_fatal("register_output_profile",                                    &
                 "Duplicate profile name: " // TRIM(profile_name))
END IF

IF ( output_spinup_local .AND. max_spinup_cycles <= 0 ) THEN
  ! If spinup output has been requested but there is no spinup, issue a warning
  ! and ignore it
  CALL log_warn("register_output_profile",                                     &
                 "Model has no spinup - ignoring request for output " //       &
                 "during spinup from profile " // TRIM(profile_name))
  output_spinup_local = .FALSE.
END IF

! Check if the above means that this profile has requested no output at all
IF ( .NOT. output_spinup_local .AND. .NOT. output_main_run ) THEN
  CALL log_warn("register_output_profile",                                     &
                 "Profile " // TRIM(profile_name) // " will provide no " //    &
                 "output with current model setup - ignoring")
  RETURN
END IF

IF ( output_main_run ) THEN
  ! If output has been requested for times that are outside the main run, we
  ! issue a warning and truncate
  IF ( output_start_local < main_run_start ) THEN
    CALL log_warn("register_output_profile",                                   &
                   "Output has been requested for times before the " //        &
                   "start of the main run - output will start at the " //      &
                   "start of the main run")
    output_start_local = main_run_start
  END IF

  IF ( output_end_local > main_run_end ) THEN
    CALL log_warn("register_output_profile",                                   &
                   "Output has been requested for times after the " //         &
                   "end of the main run - output will end at the " //          &
                   "end of the main run")
    output_end_local = main_run_end
  END IF

  ! Check that the times make sense
  IF ( output_end_local <= output_start_local )                                &
    CALL log_fatal("register_output_profile",                                  &
                   "Output cannot end before it has started." //               &
                   " Profile: " // TRIM(profile_name) )
END IF

! Check the given periods.
IF ( file_period /= period_day .AND. file_period /= period_month               &
     .AND. file_period /= period_year ) THEN
  ! Warn that one file will be used for all output, since file_period is not
  ! a special period.
  CALL log_info("register_output_profile",                                     &
                "Since file_period is not a 'special' period, all " //         &
                "output from each section will go into one file")
END IF

! Check that output_period is not period_day.
IF ( output_period == period_day ) THEN
  CALL log_fatal("register_output_profile",                                    &
                 "Output period cannot be the special period period_day. " //  &
                 "Use output period = 86400 seconds to select daily data.")
END IF

IF ( output_period /= period_month .AND. output_period /= period_year .AND.    &
     MOD(output_period, timestep_len) /= 0 )                                   &
! If output period is not a special period (monthly or yearly), it must be a
! multiple of the model timestep.
    CALL log_fatal("register_output_profile",                                  &
                   "Output period must be a 'special' period or a " //         &
                   "multiple of model timestep." //                            &
                   " Profile: " // TRIM(profile_name) )

IF ( sample_period < 1 .OR. MOD(sample_period, timestep_len) /= 0 )            &
  CALL log_fatal("register_output_profile",                                    &
                 "Sample period must be a multiple of model timestep." //      &
                 " Profile: " // TRIM(profile_name) )
! Insist that output_period is a multiple of sample_period. This test is
! designed to check cases when sample_period > timestep_len, but can always
! be applied. It is also more important for snapshot outputs which otherwise
! can be timestamped with a time other than the actual time of the data,
! which is confusing (whereas for other output types we anyway accept that
! any intermittent sampling only gives approximate results), but we impose
! this requirement for all output types for convenience and because the
! outputs will generally be better behaved with this restriction.
IF ( output_period < 1 ) THEN
  ! This is a "special" period, such as monthly, which is a whole number of
  ! days.
  ! We will check that one day is a multiple of sample_period.
  test_length = secs_in_day
ELSE
  ! We will check that output_period is a multiple of sample_period.
  test_length = output_period
END IF
IF ( MOD( test_length, sample_period ) /= 0 ) THEN
  CALL log_fatal("register_output_profile",                                    &
                 "Output period must be a multiple of sampling period. " //    &
                 "Profile: " // TRIM(profile_name) )
END IF

!-----------------------------------------------------------------------------
! Indicate what output we are providing
!-----------------------------------------------------------------------------
IF ( output_initial )                                                          &
  CALL log_info("register_output_profile",                                     &
                "Profile with name " // TRIM(profile_name) // " " //           &
                "will provide initial data for each section it is " //         &
                "outputting for")

IF ( output_spinup_local )                                                     &
  CALL log_info("register_output_profile",                                     &
                "Profile with name " // TRIM(profile_name) // " " //           &
                "registered to provide output during spinup")

IF ( output_main_run )                                                         &
  CALL log_info("register_output_profile",                                     &
                "Profile with name " // TRIM(profile_name) // " " //           &
                "registered to provide output for main run from " //           &
                datetime_to_string(output_start_local) // " to " //            &
                datetime_to_string(output_end_local))

!-----------------------------------------------------------------------------
! Set up the variable names that will be used for output
! Use user-supplied output name if given, otherwise use the model identifier
!
! We also check for duplicates in the output variable names as we go
! Note that duplicate identifiers are allowed, as long different names have
! been specified for them to use in output files
!
! Finally, if the river and land grids differ, we check whether variables are
! river or land variables, and define the output grid accordingly. We only
! allow one grid type in each profile.
!-----------------------------------------------------------------------------

have_river_variable = .FALSE.

var_names_local(:) = var_names(:)
DO i = 1,nvars

  ! Check we don't have an empty identifier.
  IF ( LEN_TRIM(identifiers(i)) == 0 ) THEN
    CALL log_fatal("register_output_profile",                                  &
                   "Missing variable name for variable #" // to_string(i) //   &
                   " detected in output profile " // TRIM(profile_name))
  END IF

  IF ( LEN_TRIM(var_names(i)) <= 0 ) var_names_local(i) = identifiers(i)

  ! If the output name being processed matches any of those previously processed,
  ! issue a fatal error
  ! We issue a fatal error to force the user to resolve the problem and avoid any
  ! confusion over what has been output
  IF ( ANY(var_names_local(1:i-1) == var_names_local(i)) )                     &
    CALL log_fatal("register_output_profile",                                  &
                   "Duplicate variable name=" // TRIM(var_names_local(i)) //   &
                   " for output files " //                                     &
                   "detected in profile " // TRIM(profile_name))

  ! Check that only the allowed types have been given as output types
  IF ( .NOT. ANY(output_types(i) == allowed_output_types) ) THEN
    CALL log_fatal("register_output_profile",                                  &
                   "Unrecognised output type '" // TRIM(output_types(i)) //    &
                   "' in profile " // TRIM(profile_name) )
  END IF

  ! A profile cannot contain both land point and river point variables as these
  ! are on different grids.
  ! Note that this is also the first point at which an unknown identifier will
  ! be detected.
  IF ( is_river_var(identifiers(i)) ) THEN
    IF ( .NOT. l_rivers ) THEN
      CALL log_fatal("register_output_profile",                                &
                     "Cannot request output on river grid if " //              &
                     "l_rivers=.FALSE." )
    END IF
    IF ( i == 1 ) THEN
      have_river_variable = .TRUE.
    ELSE IF ( .NOT. have_river_variable ) THEN
      ! This is a river variable, but we have previously found a non-river var.
      CALL log_fatal("register_output_profile",                                &
                     "Cannot mix land and river grid variables in an " //      &
                     "output file. Variable=" // TRIM(identifiers(i)) //       &
                     " in profile " // TRIM(profile_name))
    END IF
  ELSE IF ( have_river_variable ) THEN
    ! This is not a river variable, but we have previously found a river var.
    ! This is a river variable, but we have previously found a non-river var.
    CALL log_fatal("register_output_profile",                                  &
                   "Cannot mix land and river grid variables in an " //        &
                   "output file. Variable=" // TRIM(identifiers(i)) //         &
                   " in profile " // TRIM(profile_name))
  END IF

END DO

!-----------------------------------------------------------------------------
! Set up the output_profile object and its corresponding output_field objects
!-----------------------------------------------------------------------------
! Store profile constant data
nprofiles = nprofiles + 1
profiles(nprofiles)%profile_name     = profile_name
profiles(nprofiles)%output_initial   = output_initial
profiles(nprofiles)%output_spinup    = output_spinup_local
profiles(nprofiles)%output_main_run  = output_main_run
profiles(nprofiles)%file_period      = file_period
profiles(nprofiles)%output_period    = output_period
profiles(nprofiles)%sample_period    = sample_period
profiles(nprofiles)%output_start     = output_start_local
profiles(nprofiles)%output_end       = output_end_local
profiles(nprofiles)%is_river_profile = have_river_variable
profiles(nprofiles)%l_land_frac      = l_land_frac

! Allocate space on the output_profile object for the fields
profiles(nprofiles)%nfields = nvars
ALLOCATE(profiles(nprofiles)%fields(nvars))

DO i = 1,nvars
  ! Store as much info about fields as we currently know for later
  profiles(nprofiles)%fields(i)%var_id      = get_var_id(identifiers(i))
  profiles(nprofiles)%fields(i)%output_name = var_names_local(i)
  profiles(nprofiles)%fields(i)%field_type  = output_types(i)
END DO

RETURN

END SUBROUTINE register_output_profile
#endif
