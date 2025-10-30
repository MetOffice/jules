#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_agric_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_agric(frac_past_grid,frac_biocrop_grid,harvest_doy_grid,       &
                      trifctltype)

USE io_constants, ONLY: mdi, max_sdf_name_len, max_file_name_len,              &
                         namelist_unit

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE string_utils_mod, ONLY: to_string

USE input_mod, ONLY: input_grid => grid

USE jules_vegetation_mod, ONLY: l_trif_crop, l_trif_biocrop, triffid_period

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

!TYPE Definitions
USE trifctl, ONLY: trifctl_type

USE time_info_mod, ONLY: l_360
USE jules_surface_types_mod,        ONLY: npft
USE trif, ONLY: harvest_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the agricultural fraction
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments
REAL(KIND=real_jlslsm), INTENT(OUT) :: frac_past_grid(:)
                               !Fraction of pasture
REAL(KIND=real_jlslsm), INTENT(OUT) :: frac_biocrop_grid(:)
                               !Fraction of biocrop
INTEGER,                INTENT(OUT) :: harvest_doy_grid(:,:)
                        ! day of year on which biocrop harvest occurs

! TYPE Definitions
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: days_in_yr ! used to check harvest_doy
INTEGER :: n ! loop counter

!-----------------------------------------------------------------------------
! Definition of the jules_agric namelist
! For data at a single point, a single fraction is specified in the namelist
! In all other cases, agricultural fraction is read from a file
!-----------------------------------------------------------------------------
LOGICAL :: read_from_dump
LOGICAL :: read_harvest_doy_from_dump
LOGICAL :: zero_agric           ! T - set agr. frac. at all points to 0.0
                                ! F - read agr. frac. from input
LOGICAL :: zero_past            ! T - set agr. frac. at all points to 0.0
                                ! F - read agr. frac. from input
LOGICAL :: zero_biocrop         ! T - set agr. frac. at all points to 0.0
                                ! F - read agr. frac. from input

REAL(KIND=real_jlslsm) :: frac_agr                ! Single point fraction
REAL(KIND=real_jlslsm) :: frac_past               ! Single point fraction
REAL(KIND=real_jlslsm) :: frac_biocrop            ! Single point fraction

CHARACTER(LEN=max_file_name_len) :: FILE
                                ! The file to read fraction from
CHARACTER(LEN=max_sdf_name_len) :: agric_name
                                ! The name of the variable in the file
CHARACTER(LEN=max_file_name_len) :: file_past
                                ! The file to read fraction from
CHARACTER(LEN=max_sdf_name_len) :: past_name
                                ! The name of the variable in the file
CHARACTER(LEN=max_file_name_len) :: file_biocrop
                                ! The file to read fraction from
CHARACTER(LEN=max_sdf_name_len) :: biocrop_name
                                ! The name of the variable in the file
CHARACTER(LEN=errormessagelength) :: iomessage
                                ! I/O error string

CHARACTER(LEN=max_file_name_len) :: file_harvest_doy
                                ! The name of the file to read harvest_doy
CHARACTER(LEN=max_sdf_name_len) :: harvest_doy_name
                                ! The name of the variable in the file

NAMELIST  / jules_agric/ read_from_dump, zero_agric, zero_past, zero_biocrop,  &
                         frac_agr, frac_past, frac_biocrop, FILE, agric_name,  &
                         file_past, past_name, file_biocrop, biocrop_name,     &
                         read_harvest_doy_from_dump, file_harvest_doy,         &
                         harvest_doy_name

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
read_from_dump = .FALSE.
zero_agric = .TRUE.  ! Default is to set agricultural fraction to 0 everywhere
zero_past  = .TRUE.  ! Default is to set pasture fraction to 0 everywhere
zero_biocrop = .TRUE. ! Default is to set biocrop fraction to 0 everywhere
frac_agr   = mdi     ! Initialised to missing data so we can tell if it is set
                     ! using the namelist
frac_past  = mdi     ! Initialised to missing data so we can tell if it is set
                     ! using the namelist
frac_biocrop  = mdi  ! Initialised to missing data so we can tell if it is set
                     ! using the namelist
FILE=''
agric_name = ''
file_past  = ''
past_name = ''
file_biocrop = ''
biocrop_name = ''

read_harvest_doy_from_dump = .FALSE.
harvest_doy_grid(:,:) = 1
file_harvest_doy      = ''
harvest_doy_name      = 'harvest_doy'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_agric", "Reading JULES_AGRIC namelist...")

! First, we read the namelist
READ(namelist_unit, NML = jules_agric, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_agric",                                                 &
                 "Error reading namelist JULES_AGRIC " //                      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

ancil_dump_read%agric = read_from_dump

ancil_dump_read%biocrop = read_harvest_doy_from_dump

IF ( .NOT. l_trif_crop) THEN
  zero_past = .TRUE.
  zero_biocrop = .TRUE.
END IF

IF ( ancil_dump_read%agric) THEN !We read from the dump file
  CALL log_info("init_agric",                                                  &
                "agric ancils will be read from the dump file.  " //           &
                "Namelist values ignored")
ELSE !we read from the ancil file
  !---------------------------------------------------------------------------
  ! Set values derived from namelist and verify for consistency
  !---------------------------------------------------------------------------
  ! If zero fraction is selected, then that is all we have to do
  IF ( zero_agric ) THEN
    CALL log_info("init_agric", "Zero agricultural fraction indicated")
    trifctltype%frac_agr_gb(:) = 0.0
    frac_past_grid(:) = 0.0
    frac_biocrop_grid(:) = 0.0
    RETURN
  END IF

  IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
    !-------------------------------------------------------------------------
    ! If we are running a single point, read fraction from the namelist
    !-------------------------------------------------------------------------
    ! Check if frac_agr was set via the namelist.
    IF ( ABS(frac_agr - mdi) < EPSILON(frac_agr) )                             &
      CALL log_fatal("init_agric",                                             &
                     "For data at a single point, agricultural fraction "//    &
                     "is read from the namelist JULES_AGRIC")

    ! Now we know they have been set, copy their values into the model arrays
    CALL log_info("init_agric",                                                &
                  "Data is at a single point - reading agricultural " //       &
                  "fraction from namelist JULES_AGRIC")
    trifctltype%frac_agr_gb(1) = frac_agr
  ELSE
    !-------------------------------------------------------------------------
    ! If we have a grid, set fraction from the specified file
    !-------------------------------------------------------------------------
    CALL log_info("init_agric",                                                &
                  "Data is on a grid - reading agricultural fraction " //      &
                  "from file " // TRIM(FILE))

    ! Check that file name was provided
    IF ( LEN_TRIM(FILE) == 0 )                                                 &
      CALL log_fatal("init_agric", "No file name provided")

    CALL fill_variables_from_file(FILE, [ 'frac_agr' ], [ agric_name ],        &
                                  is_climatology = [ .FALSE. ] )
  END IF

  !---------------------------------------------------------------------------
  ! Check that the values seem sensible
  !---------------------------------------------------------------------------
  IF ( ANY(trifctltype%frac_agr_gb < 0.0) .OR.                                 &
       ANY(trifctltype%frac_agr_gb > 1.0) )                                    &
    CALL log_fatal("init_agric",                                               &
                   "Agricultural fraction should be in range 0.0 to 1.0, "//   &
                   "given range is " //                                        &
                   TRIM(to_string(MINVAL(trifctltype%frac_agr_gb))) //         &
                   " to " //                                                   &
                   TRIM(to_string(MAXVAL(trifctltype%frac_agr_gb))))
END IF ! ancil_dump_read%agric

!---------------------------------------------------------------------------
! Read Pasture Ancil
!---------------------------------------------------------------------------

IF ( .NOT. ancil_dump_read%agric) THEN !we read from the ancil file
  !---------------------------------------------------------------------------
  ! Set values derived from namelist and verify for consistency
  !---------------------------------------------------------------------------
  ! If zero fraction is selected, then that is all we have to do
  IF ( zero_past ) THEN
    CALL log_info("init_agric", "Zero pasture fraction indicated")
    frac_past_grid(:) = 0.0
    RETURN
  END IF

  IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
    !-------------------------------------------------------------------------
    ! If we are running a single point, read fraction from the namelist
    !-------------------------------------------------------------------------
    ! Check if frac_past was set via the namelist.
    IF ( ABS(frac_past - mdi) < EPSILON(frac_past) )                           &
      CALL log_fatal("init_agric",                                             &
                     "For data at a single point, pasture fraction "//         &
                     "is read from the namelist JULES_AGRIC")

    ! Now we know they have been set, copy their values into the model arrays
    CALL log_info("init_agric",                                                &
                  "Data is at a single point - reading pasture " //            &
                  "fraction from namelist JULES_AGRIC")
    frac_past_grid(1) = frac_past
  ELSE
    !-------------------------------------------------------------------------
    ! If we have a grid, set fraction from the specified file
    !-------------------------------------------------------------------------
    CALL log_info("init_agric",                                                &
                  "Data is on a grid - reading pasture fraction " //           &
                  "from file " // TRIM(file_past))

    ! Check that file name was provided
    IF ( LEN_TRIM(file_past) == 0 )                                            &
      CALL log_fatal("init_agric", "No file name provided")

    CALL fill_variables_from_file(file_past, [ 'frac_past' ], [ past_name ],   &
                                  is_climatology = [ .FALSE. ] )
  END IF

  !---------------------------------------------------------------------------
  ! Check that the values seem sensible
  !---------------------------------------------------------------------------
  IF ( ANY(frac_past_grid < 0.0) .OR. ANY(frac_past_grid > 1.0) )              &
    CALL log_fatal("init_agric",                                               &
                   "Pasture fraction should be in range 0.0 to 1.0, "//        &
                   "given range is " //                                        &
                   TRIM(to_string(MINVAL(frac_past_grid))) // " to " //        &
                   TRIM(to_string(MAXVAL(frac_past_grid))))

ELSE !We read from the dump file
  CALL log_info("init_agric",                                                  &
                "agric ancils will be read from the dump file.  " //           &
                "Namelist values ignored")

END IF !.NOT. ancil_dump_read%agric

!---------------------------------------------------------------------------
! Read Biocrop Ancil
!---------------------------------------------------------------------------

IF ( .NOT. ancil_dump_read%agric) THEN !we read from the ancil file
  !---------------------------------------------------------------------------
  ! Set values derived from namelist and verify for consistency
  !---------------------------------------------------------------------------
  ! If zero fraction is selected, then that is all we have to do
  IF ( zero_biocrop ) THEN
    CALL log_info("init_agric", "Zero biocrop fraction indicated")
    frac_biocrop_grid(:) = 0.0
    RETURN
  END IF

  IF ( input_grid%nx * input_grid%ny <= 1 ) THEN
    !-------------------------------------------------------------------------
    ! If we are running a single point, read fraction from the namelist
    !-------------------------------------------------------------------------
    ! Check if frac_biocrop was set via the namelist.
    IF ( ABS(frac_biocrop - mdi) < EPSILON(frac_biocrop) )                     &
      CALL log_fatal("init_agric",                                             &
                     "For data at a single point, biocrop fraction "//         &
                     "is read from the namelist JULES_AGRIC")

    ! Now we know they have been set, copy their values into the model arrays
    CALL log_info("init_agric",                                                &
                  "Data is at a single point - reading biocrop " //            &
                    "fraction from naqmelist JULES_AGRIC")
    frac_biocrop_grid(1) = frac_biocrop
  ELSE
    !-------------------------------------------------------------------------
    ! If we have a grid, set fraction from the specified file
    !-------------------------------------------------------------------------
    CALL log_info("init_agric",                                                &
                  "Data is on a grid - reading biocrop fraction " //           &
                  "from file " // TRIM(file_biocrop))

    ! Check that file name was provided
    IF ( LEN_TRIM(file_past) == 0 )                                            &
      CALL log_fatal("init_agric", "No file name provided")

    CALL fill_variables_from_file(file_biocrop, [ 'frac_biocrop' ],            &
                                 [ biocrop_name ],                             &
                                 is_climatology = [ .FALSE. ] )
  END IF

  !---------------------------------------------------------------------------
  ! Check that the values seem sensible
  !---------------------------------------------------------------------------
  IF ( ANY(frac_biocrop_grid < 0.0) .OR. ANY(frac_biocrop_grid > 1.0) )        &
    CALL log_fatal("init_agric",                                               &
                   "Biocrop fraction should be in range 0.0 to 1.0, "//        &
                   "given range is " //                                        &
                   TRIM(to_string(MINVAL(frac_biocrop_grid))) // " to " //     &
                   TRIM(to_string(MAXVAL(frac_biocrop_grid))))

ELSE !We read from the dump file
  CALL log_info("init_agric",                                                  &
                "agric ancils will be read from the dump file.  " //           &
                "Namelist values ignored")

END IF

! Initialise harvest_doy

IF (.NOT. l_trif_biocrop) RETURN

IF (.NOT. ANY( harvest_type(1:npft) == 2 ) ) RETURN

IF (.NOT. read_harvest_doy_from_dump) THEN

  !-------------------------------------------------------------------------
  ! Set harvest_doy using namelist values
  !-------------------------------------------------------------------------

  !Check that file name was provided
  IF ( LEN_TRIM(file_harvest_doy) == 0 )                                       &
    CALL log_fatal("init_agric", "No file name provided")
  CALL log_info("init_agric",                                                  &
                "Reading harvest_doy from file " // TRIM(file_harvest_doy))
  CALL fill_variables_from_file(file_harvest_doy, [ 'harvest_doy' ],           &
                                [ harvest_doy_name ],                          &
                                is_climatology = [ .FALSE. ] )


  !---------------------------------------------------------------------------
  ! Check that the values seem sensible
  !---------------------------------------------------------------------------
  IF (l_360) THEN
    days_in_yr = 360
  ELSE
    days_in_yr = 365
  END IF

  DO n = 1,npft
    IF (ANY(harvest_doy_grid(:,n) > days_in_yr - triffid_period ) .OR.         &
        ANY(harvest_doy_grid(:,n) < 1 ) ) THEN
      CALL log_fatal("init_agric", 'harvest_doy is ' //                        &
                     'too late: Reset to < (length_of_year - triffid_period)')
    END IF
  END DO


ELSE !We read from the dump file
  CALL log_info("init_agric",                                                  &
                "agric ancils will be read from the dump file.  " //           &
                "Namelist values ignored")

END IF

RETURN

END SUBROUTINE init_agric

END MODULE init_agric_mod
#endif
