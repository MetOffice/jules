#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE init_irrig_props_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_irrig_props()

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len, namelist_unit

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     imdi, rmdi

USE model_interface_mod, ONLY: populate_var, get_var_id

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE string_utils_mod, ONLY: to_string

USE jules_irrig_mod, ONLY: l_irrig_dmd,  irrtiles, frac_irrig_all_tiles,       &
                           nirrtile, irrigtiles,                               &
                           set_irrfrac_on_irrtiles

USE dump_mod, ONLY: ancil_dump_read

USE jules_surface_types_mod, ONLY: npft

USE um_types, ONLY: real_jlslsm

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!    Initialises irrigation parameters and properties
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER :: ERROR  ! Error indicator

INTEGER :: i  ! Loop counter

CHARACTER(LEN=50000) :: lineBuffer

!-----------------------------------------------------------------------------
! Definition of the jules_irrig_props namelist
!-----------------------------------------------------------------------------
LOGICAL :: read_from_dump
CHARACTER(LEN=max_file_name_len) :: irrig_frac_file
                      ! The name of the file to use for irrigation fraction
                      ! if read_file = .false.
LOGICAL :: read_file
                      !   T - read from file
                      !   F - use a constant value for all points
CHARACTER(LEN=max_sdf_name_len) :: var_name
                      ! The name of the variable in the file
REAL(KIND=real_jlslsm) :: const_frac_irr = rmdi
                      ! The constant value to use for each variable if
                      ! read_file = F and frac_irrig_all_tiles = T
                      ! for that variable
REAL :: const_irrfrac_irrtiles = rmdi
                      ! The constant value to use for each variable if
                      ! read_file = F and set_irrig_on_irrtiles = T
                      ! for that variable

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_IRRIG_PROPS'

NAMELIST  /jules_irrig_props/ read_from_dump, irrig_frac_file, read_file,      &
                       var_name, const_frac_irr, const_irrfrac_irrtiles

!-----------------------------------------------------------------------------

! Nothing to do if irrigation demand model is not on
IF ( .NOT. l_irrig_dmd ) RETURN

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
read_from_dump = .FALSE.
read_file = .TRUE.       ! Default is to read every variable from file
irrig_frac_file =''      ! Empty file name
var_name  = ''           ! Empty variable name.

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_IRRIG_PROPS namelist...")

READ(namelist_unit, NML = jules_irrig_props, IOSTAT = ERROR)

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                "Error reading namelist JULES_IRRIG_PROPS " //                 &
                "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

! Print the contents of the namelist_unit
CALL jules_print(RoutineName, 'Contents of namelist init_irrig_props')

WRITE(lineBuffer,*) ' read_from_dump = ',read_from_dump
CALL jules_print(RoutineName,lineBuffer)

WRITE(lineBuffer,*) ' irrig_frac_file = ',irrig_frac_file
CALL jules_print(RoutineName,lineBuffer)

WRITE(lineBuffer,*) ' read_file = ',read_file
CALL jules_print(RoutineName,lineBuffer)

WRITE(lineBuffer,*) ' var_name = ',var_name
CALL jules_print(RoutineName,lineBuffer)

WRITE(lineBuffer,*) ' const_frac_irr = ',const_frac_irr
CALL jules_print(RoutineName,lineBuffer)

WRITE(lineBuffer,*) ' const_irrfrac_irrtiles = ',const_irrfrac_irrtiles
CALL jules_print(RoutineName,lineBuffer)

ancil_dump_read%irrig = read_from_dump

IF ( .NOT. ancil_dump_read%irrig) THEN !we read from the ancil file

  !---------------------------------------------------------------------------
  ! Determine if variable to be read from file
  !---------------------------------------------------------------------------
  IF ( read_file ) THEN
    CALL log_info(RoutineName, "'frac_irrig' will be read from file")

    ! If the variable will be filled from file, register it here
    IF ( LEN_TRIM(irrig_frac_file) == 0 )                                      &
      CALL log_fatal(RoutineName, "No file name provided")

    CALL fill_variables_from_file(irrig_frac_file, [ 'frac_irrig' ],           &
       [ var_name ], is_climatology = [ .FALSE. ])
  ELSE
    ! If the variable is being set as a constant, populate it here.
    IF ( set_irrfrac_on_irrtiles ) THEN
       ! The irrigation fraction is specified for irrigated tiles
       ! using irrfrac_irrtiles
       ! First check that a value has been provided.
      IF ( ABS( const_irrfrac_irrtiles - rmdi ) < EPSILON(1.0) )               &
        CALL log_fatal(RoutineName,                                            &
                  "No constant value provided for irrfrac_irrtiles.")
      CALL log_info(RoutineName,                                               &
               "'const_irrfrac_irrtiles' will be set to a constant = " //      &
                        to_string(const_irrfrac_irrtiles))
      CALL populate_var(get_var_id('irrfrac_irrtiles'),                        &
                                        const_val = const_irrfrac_irrtiles)
    ELSE
       ! The irrigated fraction is set as const_frac_irr from namelist.
       ! First check that a value has been provided.
      IF ( ABS( const_frac_irr - rmdi ) < EPSILON(1.0) )                       &
        CALL log_fatal(RoutineName,                                            &
                     "No constant value provided for frac_irrig.")

      CALL log_info(RoutineName,                                               &
                  "'frac_irrig' will be set to a constant = " //               &
                  to_string(const_frac_irr))

      CALL populate_var(get_var_id('frac_irrig'), const_val = const_frac_irr)
    END IF
  END IF

  !---------------------------------------------------------------------------
  ! Process pft names to be assigned irr fraction (if not all)
  !---------------------------------------------------------------------------
  IF ( .NOT. frac_irrig_all_tiles ) THEN
    IF ( ( nirrtile > npft ) .OR. ( nirrtile < 1 ) ) THEN
      CALL log_info(RoutineName,                                               &
                    "nirrtile should be greater than 1 and less than or" //    &
                    "equal to the number of pfts.")
    END IF
    ! Copy pft indices from namelist into allocated array.
    ! First check that values were given.
    IF ( ANY( irrigtiles(1:nirrtile) == imdi ) )                               &
      CALL log_fatal(RoutineName,                                              &
                     "Insufficient values provided for irrigtiles.")

    ! irrtiles is now allocated with SIZE(npft) instead of SIZE(npft_max).
    ! It should really be allocated with SIZE(nirrtile) though, this will
    ! be changed through ticket jules:#1065
    irrtiles(1:nirrtile) = irrigtiles(1:nirrtile)
  END IF

ELSE !We read from the dump file
  CALL log_info(RoutineName,                                                   &
                "Irrigation ancil information will be read from the dump." //  &
                "Namelist values ignored")
END IF !.NOT. ancil_dump_read%irrig

END SUBROUTINE init_irrig_props
END MODULE init_irrig_props_mod
#endif
