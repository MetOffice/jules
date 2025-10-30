#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_drive_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_drive(nml_dir,ainfo,progs_data)

USE conversions_mod, ONLY: isec_per_day, rsec_per_hour


USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE missing_data_mod, ONLY: imdi, rmdi

USE model_interface_mod, ONLY: identifier_len

USE model_time_mod, ONLY: timesteps_in_day, main_run_start, spinup_start

USE datetime_mod, ONLY: datetime_str_len, datetime, datetime_from_string

USE string_utils_mod, ONLY: to_string

USE templating_mod, ONLY: tpl_detect_period, tpl_has_var_name,                 &
                           tpl_substitute_var

USE update_mod, ONLY: io_precip_type, io_rad_type, io_wind_speed,              &
                       use_diff_rad, diff_frac_const, t_for_con_rain,          &
                       t_for_snow,                                             &
                       l_imogen, l_daily_disagg, l_disagg_const_rh,            &
                       dur_ls_rain, dur_conv_rain,                             &
                       dur_ls_snow, dur_conv_snow, precip_disagg_method,       &
                       l_perturb_driving, temperature_abs_perturbation,        &
                       precip_rel_perturbation, bl_height

USE time_varying_input_mod, ONLY: register_input_file

USE jules_surface_mod, ONLY: l_point_data

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE disaggregated_precip, ONLY: ls_rain_disagg, con_rain_disagg,               &
                                 ls_snow_disagg, con_snow_disagg

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

!TYPE definitions
USE ancil_info, ONLY: ainfo_type
USE prognostics, ONLY: progs_data_type

USE jules_model_environment_mod, ONLY: lsm_id, rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about driving data and initialises it
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing the data
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(progs_data_type), INTENT(IN OUT) :: progs_data

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_DRIVE'

! Work variables
INTEGER, PARAMETER :: max_drive_vars = 20

INTEGER :: nvars_required      ! The number of variables that are
                               ! required in the current configuration
CHARACTER(LEN=identifier_len) :: required_vars(max_drive_vars)
                               ! The variable identifiers of the required
                               ! variables

INTEGER :: nvars_file  ! The number of files to be read from file

TYPE(datetime) :: data_start_dt, data_end_dt
                            ! Start and end times for data as datetime objects

LOGICAL :: use_time_tpl     ! Determines whether we will be using time
                            ! templating or whether we will pass lists
                            ! of files and times to register_input_file

LOGICAL :: use_var_name_tpl  ! Determines whether we will be using variable
                             ! name templating or not

CHARACTER(LEN=max_file_name_len), ALLOCATABLE :: file_names(:)
                            ! If use_time_tpl=T, the list of file names
TYPE(datetime), ALLOCATABLE :: file_times(:)
                            ! If use_time_tpl=T, the list of file start times
CHARACTER(LEN=datetime_str_len) :: file_time_str
                            ! The read file time as a string until it can
                            ! be converted

INTEGER :: nseed  ! The size of the random seed if using the disaggregator

INTEGER :: i,j  ! Index variables

INTEGER :: ERROR, error_sum  ! Error indicators
CHARACTER(LEN=errormessagelength) :: iomessage

INTEGER :: nvars_radiation       ! Counter to hold the number of radiation
                                 ! variables specified at the namelist

!-----------------------------------------------------------------------------
! Definition of the jules_drive namelist
!-----------------------------------------------------------------------------
! Information about the data in the file
CHARACTER(LEN=datetime_str_len) :: data_start, data_end
                           ! Start and end times for driving data as strings
INTEGER :: data_period     ! The period of the driving data

! Properties that determine how dependent variables are updated

! Information about the files to use
LOGICAL :: read_list       ! T - the given file contains a list of file
                           !     names and times of first data
                           !     These files may contain variable name
                           !     templating but not time templating
                           ! F - driving data should be read directly from
                           !     the given file
                           !     The file may contain variable and time
                           !     templating
INTEGER :: nfiles          ! The number of files/file times to read from the
                           ! list file
                           ! ONLY USED IF read_list=T
CHARACTER(LEN=max_file_name_len) :: FILE
                           ! The file to use for whatever read_list
                           ! determines

! Information about the variables contained in the file
INTEGER :: nvars
CHARACTER(LEN=identifier_len) :: var(max_drive_vars)
                      ! The variable identifiers of the variables
CHARACTER(LEN=max_sdf_name_len) :: var_name(max_drive_vars)
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(max_drive_vars)
                      ! The name to substitute in a template for each
                      ! variable
CHARACTER(LEN=2) :: interp(max_drive_vars)
                      ! Flag indicating the type of interpolation to use
                      ! for each variable

LOGICAL :: z1_tq_vary = .FALSE.
    !   Switch to control how the height that the temperature and humidity
    !   driving data is valid at (z1_tq) is set
    !   T means that this height is read from file
    !   F means this height is constant for each point and given by z1_tq_in

REAL(KIND=real_jlslsm) :: z1_uv_in, z1_tq_in
    ! values of z1_uv and z1_tq that are used at all points on grid

CHARACTER(LEN=max_file_name_len) :: z1_tq_file
                      ! The name of the file containing z1_tq

CHARACTER(LEN=max_sdf_name_len) :: z1_tq_var_name
                      ! The name of the variable containing z1_tq in the file


NAMELIST  / jules_drive/ l_imogen, l_daily_disagg,                             &
                       t_for_snow, t_for_con_rain,                             &
                       diff_frac_const,                                        &
                       data_start, data_end, data_period,                      &
                       read_list, nfiles, FILE,                                &
                       nvars, var, var_name, tpl_name, interp,                 &
                       l_disagg_const_rh, precip_disagg_method,                &
                       dur_ls_rain, dur_conv_rain,                             &
                       dur_ls_snow, dur_conv_snow,                             &
                       z1_tq_vary, z1_uv_in, z1_tq_in,                         &
                       z1_tq_file, z1_tq_var_name,                             &
                       l_perturb_driving, temperature_abs_perturbation,        &
                       precip_rel_perturbation, bl_height

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
nvars_required = 0
nvars_file     = 0
read_list      = .FALSE.  ! Default is to assume that the given file name
                          ! either provides the data or is a template
nfiles         = 0
nvars          = 0
FILE=''       ! Empty file name.
var(:)         = ''       ! Empty identifiers.
var_name(:)    = ''       ! Empty variable names.
tpl_name(:)    = ''       ! Empty template name.
interp(:)      = ''       ! Empty interpolation flag.
data_start     = ''       ! Empty
data_end       = ''       ! Empty
data_period    = imdi     ! Missing data value
z1_tq_file     = ''
z1_tq_var_name = ''
z1_uv_in       = rmdi     ! Missing data value
z1_tq_in       = rmdi     ! Missing data value

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_DRIVE namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'drive.nml'),                &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file drive.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_drive, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_DRIVE " //                      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file drive.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Check that variable identifiers are not empty.
! Although we might later decide that the identifier is not required, for
! clarity we check here whether the claimed amount of information was
! provided.
!-----------------------------------------------------------------------------
DO i = 1,nvars
  IF ( LEN_TRIM(var(i)) == 0 )                                                 &
    CALL log_fatal(RoutineName,                                                &
                   "Insufficient values for var. " //                          &
                   "No name provided for var at position #" //                 &
                   TRIM(to_string(i)) )
END DO

!-----------------------------------------------------------------------------
! Set the reference level heights for wind and air temperature/humidity to
! user-specified values, after checking that values were provided.
!-----------------------------------------------------------------------------
IF ( ABS( z1_uv_in - rmdi ) < EPSILON(1.0) ) THEN
  CALL log_fatal(RoutineName, "No value given for z1_uv_in")
END IF
ainfo%z1_uv_ij(:,:) = z1_uv_in

IF ( z1_tq_vary ) THEN
  IF ( LEN_TRIM(z1_tq_file) == 0 )                                             &
      CALL log_fatal(RoutineName, "No file name provided for z1_tq_file.")
  CALL fill_variables_from_file(                                               &
    z1_tq_file, [ 'z1_tq_in' ], [ z1_tq_var_name ],                            &
    is_climatology = [ .FALSE. ]                                               &
    )
ELSE
  IF ( ABS( z1_tq_in - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for z1_tq_in")
  END IF
  ainfo%z1_tq_ij(:,:) = z1_tq_in
END IF

!-----------------------------------------------------------------------------
! Check if IMOGEN will be used to update the main driving variables
!-----------------------------------------------------------------------------
IF ( l_imogen ) THEN

  CALL log_info(RoutineName, "Driving data will be provided by IMOGEN")

  ! Using IMOGEN to update driving variables is conceptually the same as having
  ! sw_down and lw_down provided (io_rad_type = 1), all 4 precip components
  ! provided (io_precip_type = 4) and both components of wind provided
  ! (io_wind_speed = F)
  ! We also use a constant diffuse fraction
  io_rad_type = 1
  io_precip_type = 4
  io_wind_speed = .FALSE.
  use_diff_rad = .FALSE.
  ! Set precipitation event durations. Note that IMOGEN does not use
  ! dur_conv_snow.
  dur_conv_rain = 6.0 * rsec_per_hour
  dur_ls_rain   = rsec_per_hour
  dur_ls_snow   = rsec_per_hour

  IF ( l_daily_disagg ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "l_daily_disagg = T cannot not be used with " //            &
                   "l_imogen = T")
  END IF

  IF ( l_perturb_driving )                                                     &
    CALL log_fatal(RoutineName,                                                &
                   "if IMOGEN is on, can not set l_perturb_driving=T")

ELSE

  !---------------------------------------------------------------------------
  ! IMOGEN is not being used.
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Convert data_start and data_end to datetime objects
  !---------------------------------------------------------------------------
  ! Check that the dates have been given.
  IF ( LEN_TRIM(data_start) == 0 .OR. LEN_TRIM(data_end) == 0 )                &
      CALL log_fatal(RoutineName,                                              &
                     "data_start and data_end must both be specified")
  data_start_dt = datetime_from_string(data_start)
  data_end_dt   = datetime_from_string(data_end)

  !---------------------------------------------------------------------------
  ! Check data_period was given.
  !---------------------------------------------------------------------------
  IF ( data_period == imdi )                                                   &
      CALL log_fatal(RoutineName,                                              &
                     "data_period must be specified")

  !---------------------------------------------------------------------------
  ! If using the disaggregator, check that the configuration is acceptable
  !---------------------------------------------------------------------------
  IF ( l_daily_disagg ) THEN

    IF ( data_period /= isec_per_day ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "Data period must 86400 seconds (i.e. daily data) " //    &
                     "if l_daily_disagg = T")
    END IF

    IF ( .NOT. ANY(var(1:nvars) == 'sw_down') .OR.                             &
         .NOT. ANY(var(1:nvars) == 'lw_down') )                                &
      CALL log_fatal(RoutineName,                                              &
                     "Both sw_down and lw_down must be provided directly " //  &
                     "if l_daily_disagg = T")

    DO i = 1,nvars
      IF ( var(i) == 'ls_rain'  .OR. var(i) == 'con_rain' .OR.                 &
           var(i) == 'ls_snow'  .OR. var(i) == 'con_snow' .OR.                 &
           var(i) == 'tot_rain' .OR. var(i) == 'tot_snow' .OR.                 &
           var(i) == 'precip' ) THEN
        IF ( interp(i) /= 'nf' .AND. interp(i) /= 'f' ) THEN
          CALL log_fatal(RoutineName,                                          &
                         "If l_daily_disagg=T, precip forcing variables " //   &
                         "should have interp = nf or f")
        END IF
      END IF
    END DO

    IF ( main_run_start%TIME /= 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "If l_daily_disagg = T, main run start should be " //     &
                     "00:00:00 for some day")
    END IF

    IF ( spinup_start%TIME /= 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "If l_daily_disagg = T, spinup start should be " //       &
                     "00:00:00 for some day")
    END IF

    IF ( data_start_dt%TIME /= 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "If l_daily_disagg = T, driving data should start at " // &
                     "00:00:00 for some day")
    END IF

    ! Configuration is fine, so tell the user
    CALL log_info(RoutineName, "Using daily disaggregator")
    IF ( l_disagg_const_rh ) CALL log_info(RoutineName, "Using constant RH.")

    !-------------------------------------------------------------------------
    ! Allocate the necessary arrays
    !-------------------------------------------------------------------------
    error_sum = 0
    ALLOCATE(ls_rain_disagg(t_i_length,t_j_length,timesteps_in_day),           &
             STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(con_rain_disagg(t_i_length,t_j_length,timesteps_in_day),          &
             STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(ls_snow_disagg(t_i_length,t_j_length,timesteps_in_day),           &
             STAT = ERROR)
    error_sum = error_sum + ERROR
    ALLOCATE(con_snow_disagg(t_i_length,t_j_length,timesteps_in_day),          &
             STAT = ERROR)
    error_sum = error_sum + ERROR

    ! Check that precipitation disaggregation method was given and is valid.
    IF ( precip_disagg_method == imdi ) THEN
      CALL log_fatal(RoutineName, "precip_disagg_method must be specified")
    ELSE
      SELECT CASE ( precip_disagg_method )
      CASE ( 1, 2, 3, 4 )
        ! Valid options; nothing to do.
      CASE DEFAULT
        CALL log_fatal(RoutineName,                                            &
                       "precip_disagg_method must be 1, 2, 3 or 4")
      END SELECT
    END IF

    IF ( precip_disagg_method > 1 ) THEN
      ! Find the number of integers used to hold the random seed and allocate
      CALL RANDOM_SEED( SIZE = nseed )
      ALLOCATE( progs_data%seed_rain(nseed), STAT = ERROR )
      error_sum = error_sum + ERROR
    END IF

    ! Check for error.
    IF ( error_sum /= 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "Error allocating arrays for daily disaggregator")
    ELSE
      ls_rain_disagg(:,:,:)  = 0.0
      con_rain_disagg(:,:,:) = 0.0
      ls_snow_disagg(:,:,:)  = 0.0
      con_snow_disagg(:,:,:) = 0.0
    END IF

  END IF  !  l_daily_disagg)

  !---------------------------------------------------------------------------
  ! If we get to here, we will be reading data from file, so we need to check
  ! that a file name was given
  !---------------------------------------------------------------------------
  IF ( LEN_TRIM(FILE) == 0 )                                                   &
    CALL log_fatal(RoutineName, "No file name provided")

  !---------------------------------------------------------------------------
  ! Work out what files we will be using to read driving data
  !---------------------------------------------------------------------------
  IF ( read_list ) THEN
    IF ( nfiles < 1 )                                                          &
      CALL log_fatal(RoutineName,                                              &
                     "If reading a list of file names and file times, at " //  &
                     "least one file must be given")

    ! If we are reading a list of files, then we will definitely not be using
    ! time templating
    use_time_tpl = .FALSE.

    !-------------------------------------------------------------------------
    ! Read the list of file names and times from the file
    !-------------------------------------------------------------------------
    CALL log_info(RoutineName,                                                 &
                  "Reading list of drive file names and start times...")

    ! First allocate space for the elements
    ALLOCATE(file_names(nfiles), STAT = ERROR)
    error_sum = ERROR
    ALLOCATE(file_times(nfiles), STAT = ERROR)
    error_sum = error_sum + ERROR
    IF ( error_sum /= 0 )                                                      &
      CALL log_fatal(RoutineName,                                              &
                     "Error allocating arrays for file names and times")

    ! Open the file
    OPEN(namelist_unit, FILE=FILE, STATUS='old', POSITION='rewind',            &
                        ACTION='read', IOSTAT = ERROR, IOMSG = iomessage)
    IF ( ERROR /= 0 )                                                          &
      CALL log_fatal(RoutineName,                                              &
                     "Error opening file " // TRIM(FILE) // " " //             &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")

    ! Read the number of files that we have been told to read
    DO i = 1,nfiles
      READ(namelist_unit, *, IOSTAT = ERROR, IOMSG = iomessage) file_names(i), &
                                                            file_time_str
      IF ( ERROR /= 0 )                                                        &
        CALL log_fatal(RoutineName,                                            &
                       "Error reading file name/time pair from file " //       &
                       "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //    &
                       TRIM(iomessage) // ")")
      ! Convert the file time as a string into a datetime object
      file_times(i) = datetime_from_string(file_time_str)
    END DO

    ! Close the file
    CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
    IF ( ERROR /= 0 )                                                          &
      CALL log_fatal(RoutineName,                                              &
                     "Error closing file " // TRIM(FILE) // " " //             &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")
  ELSE
    ! If we are not using a list of files, then we detect if we are using time
    ! templating or not
    use_time_tpl = ( tpl_detect_period(FILE) < 0 )

    IF ( use_time_tpl ) THEN
      CALL log_info(RoutineName,                                               &
                    "Using time templating to get drive file names")
    ELSE
      CALL log_info(RoutineName,                                               &
                    "Using single file for all driving data times")
    END IF

    ! If we are not using templating, then we give lists indicating one file
    ! that starts at the data start time to register_input_file
    ALLOCATE(file_names(1), STAT = ERROR)
    error_sum = ERROR
    ALLOCATE(file_times(1), STAT = ERROR)
    error_sum = error_sum + ERROR
    IF ( error_sum /= 0 )                                                      &
      CALL log_fatal(RoutineName,                                              &
                     "Error allocating arrays for file names and times")

    file_names(1) = FILE
    file_times(1) = data_start_dt
  END IF

  ! Check if we are going to be using variable name templating or not
  IF ( use_time_tpl ) THEN
    ! If using time templating, just check the given file name for variable
    ! templating
    use_var_name_tpl = tpl_has_var_name(FILE)
  ELSE
    ! If using a list of files, check that they either all have variable
    ! templating or all don't)

    ! Check the first file first
    use_var_name_tpl = tpl_has_var_name(file_names(1))

    DO i = 2,nfiles
      ! Check that the rest of the file names match
      IF ( use_var_name_tpl .NEQV. tpl_has_var_name(file_names(i)) )           &
        CALL log_fatal(RoutineName,                                            &
                       "If providing a list of files, either they must " //    &
                       "all use variable name templating or all NOT use " //   &
                       "variable name templating")
    END DO
  END IF

  IF ( use_var_name_tpl )                                                      &
    CALL log_info(RoutineName,                                                 &
                  "Using variable name templating to get drive file names")


  !---------------------------------------------------------------------------
  ! Build the list of required driving variables
  !---------------------------------------------------------------------------
  ! First, variables that are always read in directly - they can't be derived
  nvars_required = 3
  required_vars(1:3) = [ 'pstar', 'q    ', 't    ' ]

  IF ( l_daily_disagg ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'dt_range'
  END IF

  !---------------------------------------------------------------------------
  ! Radiation
  ! Currently we always require downward fluxes, but these can be
  ! derived from net fluxes. This case is rather unusual in that the
  ! net fluxes are stored in the variables used for downward fluxes, until
  ! the downward fluxes are calculated in subroutine CONTROL.
  ! So although we require the downward fluxes, we may not save them
  ! at this level of code - waits until CONTROL.
  ! CONTROL uses the value of io_rad_type to decide what to do with the fluxes
  ! We can detect what value of io_rad_type to use by looking at the given
  ! variables
  !---------------------------------------------------------------------------

  SELECT CASE ( lsm_id )
  CASE ( rivers )
    ! Standalone Rivers only uses a few driving variables as it is run
    ! independently of the rest of JULES.
    nvars_required = 2
    required_vars(1:2) = [ 'sub_surf_roff', 'surf_roff    ' ]
  CASE DEFAULT
    IF ( ANY(var(1:nvars) == 'lw_down') .AND.                                  &
         ANY(var(1:nvars) == 'sw_down')  ) THEN
      CALL log_info(RoutineName,                                               &
                    "Downward LW and downward SW radiation are both " //       &
                    "provided directly")

      ! The default is to use io_rad_type=1 - downward lw and sw are given
      ! directly
      io_rad_type = 1

      ! io_rad_type = 1 requires downward longwave and downward shortwave
      ! radiation
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'lw_down'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'sw_down'

    ELSE IF ( ANY(var(1:nvars) == 'rad_net') .AND.                             &
              ANY(var(1:nvars) == 'sw_down')) THEN
      CALL log_info(RoutineName,                                               &
                    "Downward SW and downward LW radiation will be derived " //&
                    "from net downward all-wavelength and downward SW "      //&
                    "radiation")

      ! If net downward all-wavelength radiation is given, we need./ to use
      ! io_rad_type=2
      io_rad_type = 2

      ! io_rad_type = 2 requires net downward radiation and sw down radiation
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'rad_net'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'sw_down'

    ELSE IF ( ANY(var(1:nvars) == 'lw_net') .AND.                              &
              ANY(var(1:nvars) == 'sw_net')  ) THEN
      CALL log_info(RoutineName,                                               &
                    "Downward SW and downward LW radiation will be derived " //&
                    "from net downward LW and net downward SW radiation")

      ! If net downward longwave and shortwave radiations are given, we need to
      ! use io_rad_type = 3
      io_rad_type = 3

      ! io_rad_type = 3 requires net downward lw and net downward sw
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'lw_net'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'sw_net'

    ELSE IF ( ANY(var(1:nvars) == 'lw_down') .AND.                             &
              ANY(var(1:nvars) == 'sw_net')  ) THEN
      CALL log_info(RoutineName,                                               &
            "Downward LW radiation is provided directly and "                //&
            "downward SW will be derived from net downward SW radiation")

      ! If downward shortwave radiation and net downward longwave radiations are
      !  given, we need to use io_rad_type = 4
      io_rad_type = 4

      ! io_rad_type = 4 requires downward longwave and net downward shortwave
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'lw_down'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'sw_net'

    ELSE

      ! We raise a fatal error if none of the appropriate forcing pairs are
      ! present.

      CALL log_fatal(RoutineName,  "Not enough radiation variables or "    //  &
                     "invalid radiation forcing pair at drive.nml")

    END IF

    ! Check the number of radiation variables
    nvars_radiation = 0
    IF (ANY(var(1:nvars) == 'rad_net')) nvars_radiation = nvars_radiation + 1
    IF (ANY(var(1:nvars) == 'sw_down')) nvars_radiation = nvars_radiation + 1
    IF (ANY(var(1:nvars) == 'sw_net'))  nvars_radiation = nvars_radiation + 1
    IF (ANY(var(1:nvars) == 'lw_down')) nvars_radiation = nvars_radiation + 1
    IF (ANY(var(1:nvars) == 'lw_net'))  nvars_radiation = nvars_radiation + 1

    ! Fatal error if there are too many radiation variables. This
    ! is to prevent different results if three radiation forcing
    ! variables are specified in different order in two model runs.
    IF (nvars_radiation > 2) THEN
      CALL log_fatal (RoutineName,  "Too many radiation variables " //         &
                      "specified at drive.nml")
    END IF

    ! Check if diffuse radiation will be supplied from file
    ! Otherwise we will use the given constant
    IF ( ANY(var(1:nvars) == 'diff_rad') ) THEN
      CALL log_info(RoutineName, "Diffuse radiation will be read from file")

      use_diff_rad = .TRUE.

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'diff_rad'
    ELSE
      CALL log_info(RoutineName, "Diffuse radiation will be set as a constant")
      IF ( diff_frac_const == rmdi ) THEN
        CALL log_fatal(RoutineName,                                            &
                       "Diffuse radiation constant (diff_frac_const) is unset")
      ELSE IF ( diff_frac_const == 0.0 ) THEN
        CALL log_warn(RoutineName,                                             &
                      "No diffuse shortwave radiation as diff_frac_const = 0.0")
      END IF
    END IF

    !---------------------------------------------------------------------------
    ! Precipitation
    ! Currently we always require four components (large scale/convective
    ! solid/liquid precip), but these can be derived from totals
    !---------------------------------------------------------------------------
    IF ( ANY(var(1:nvars) == 'precip') ) THEN
      CALL log_info(RoutineName,                                               &
                    "Precipitation components will be derived from total " //  &
                    "precipitation")

      ! If total precip is given, we need io_precip_type=1
      io_precip_type = 1

      ! The only required variable is the total precip
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'precip'

    ELSE IF ( ANY(var(1:nvars) == 'tot_rain') ) THEN
      CALL log_info(RoutineName,                                               &
                    "Precipitation components will be derived from total " //  &
                    "rainfall and total snowfall")

      ! If total rainfall is given, we need io_precip_type=2
      io_precip_type = 2

      ! We require total rain and total snow to be given
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'tot_rain'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'tot_snow'

    ELSE IF ( ANY(var(1:nvars) == 'tot_snow') ) THEN
      CALL log_info(RoutineName,                                               &
                    "Precipitation components will be derived from " //        &
                    "convective rainfall, large-scale rainfall and total " //  &
                    "snowfall")

      ! If we are given total snow without total rain (checked above), we need
      ! io_precip_type = 3
      io_precip_type = 3

      ! This requires rainfall components to be given separately but total
      ! snowfall
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'con_rain'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'ls_rain'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'tot_snow'

    ELSE
      CALL log_info(RoutineName,                                               &
                    "All precipitation components are provided directly")

      ! The default is to assume all four components are input directly
      ! This is io_precip_type=4
      io_precip_type = 4

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'con_rain'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'ls_rain'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'con_snow'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'ls_snow'
    END IF

    !-----------------------------------------------------------------------------
    ! Wind
    ! Currently we always require two components, but these can be set from the
    ! total
    !-----------------------------------------------------------------------------
    IF ( ANY(var(1:nvars) == 'wind') ) THEN
      CALL log_info(RoutineName,                                               &
                    "Horizontal components of wind will be derived from " //   &
                    "wind speed")

      ! If total wind is provided, then use it
      io_wind_speed = .TRUE.

      ! The total wind speed is the only required variable
      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'wind'

    ELSE
      CALL log_info(RoutineName,                                               &
                    "Horizontal components of wind given directly")

      ! Otherwise we require both components to be given
      io_wind_speed = .FALSE.

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'u'

      nvars_required = nvars_required + 1
      required_vars(nvars_required) = 'v'
    END IF
  END SELECT

  !---------------------------------------------------------------------------
  ! Check that all the required variables have been given
  !---------------------------------------------------------------------------
  DO i = 1,nvars_required
    IF ( .NOT. ANY(var(1:nvars) == required_vars(i)) )                         &
      CALL log_fatal(RoutineName,                                              &
                     "Could not find required variable '" //                   &
                     TRIM(required_vars(i)) // "' in list")
  END DO

  !---------------------------------------------------------------------------
  ! Check which given variables we will be using
  !---------------------------------------------------------------------------
  DO i = 1,nvars
    !-------------------------------------------------------------------------
    ! If the variable is one of the required vars, then we will be using it
    !-------------------------------------------------------------------------
    IF ( ANY(required_vars(1:nvars_required) == var(i)) ) THEN
      CALL log_info(RoutineName,                                               &
                    "'" // TRIM(var(i)) // "' will be read from file")

      ! If the variable will be filled from file, register it here
      nvars_file = nvars_file + 1
      ! Since nvars_file <= i (so we will not overwrite unprocessed values)
      ! and we do not need the values from these arrays for any non-file
      ! variables from now on, we can just compress them down onto variables
      ! that are in the file
      var(nvars_file)      = var(i)
      var_name(nvars_file) = var_name(i)
      tpl_name(nvars_file) = tpl_name(i)
      interp(nvars_file)   = interp(i)

      IF ( LEN_TRIM(interp(nvars_file)) == 0 )                                 &
        CALL log_fatal(RoutineName,                                            &
                     "No value given for interp (interpolation flag) for " //  &
                     "variable " // TRIM(var(nvars_file)) )

    ELSE
      ! If the variable is not a required variable, warn about not using it
      CALL log_warn(RoutineName,                                               &
                    "Provided variable '" // TRIM(var(i)) //                   &
                    "' is not required, so will be ignored")
    END IF
  END DO

  !---------------------------------------------------------------------------
  ! Register the input file(s)
  !---------------------------------------------------------------------------
  IF ( use_var_name_tpl ) THEN
    ! If we are using a variable name template, we must loop through the
    ! variables and register one file (set of files) per variable
    DO i = 1,nvars_file
      ! Check that a template string was provided for this variable.
      IF ( LEN_TRIM(tpl_name(i)) == 0 )                                        &
        CALL log_fatal( RoutineName,                                           &
                        "No variable name template substitution " //           &
                        "provided for " // TRIM(var(i)) )
      CALL register_input_file(data_start_dt, data_end_dt, data_period,        &
      ! Driving data should not be a climatology
                                     .FALSE.,                                  &
                                     use_time_tpl,                             &
      ! Substitute the variable name into the file
                                     tpl_substitute_var(FILE, tpl_name(i)),    &
      ! Build the list of file names using an array comprehension by
      ! substituting the variable name into each file name
                                     [ (tpl_substitute_var(                    &
                                           file_names(j), tpl_name(i)          &
                                     ), j = 1,SIZE(file_names)) ],             &
                                     file_times,                               &
      ! Use array constructors to give the single values related to the
      ! variable
                                     [ var(i) ], [ var_name(i) ],              &
                                     [ interp(i) ])
    END DO

  ELSE
    ! We are not using variable name templating, so register the same file as
    ! providing all variables
    CALL register_input_file(data_start_dt, data_end_dt, data_period,          &
    ! Driving data should not be a climatology
                                 .FALSE.,                                      &
                                 use_time_tpl, FILE, file_names, file_times,   &
                                 var(1:nvars_file), var_name(1:nvars_file),    &
                                 interp(1:nvars_file))
  END IF

  DEALLOCATE(file_names)
  DEALLOCATE(file_times)

  !---------------------------------------------------------------------------
  ! Check precipitation variables are consistent with l_point_data.
  !---------------------------------------------------------------------------
  IF ( l_point_data .AND.                                                      &
       ( io_precip_type == 3 .OR. io_precip_type == 4 ) ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Precipitation variables are not consistent with " //       &
                   "l_point_data=TRUE. Either provide precip or tot_rain " //  &
                   "and tot_snow.")
  END IF

END IF  !  l_imogen

!-----------------------------------------------------------------------------
! Check event durations were provided and reasonable.
!-----------------------------------------------------------------------------
IF ( l_daily_disagg ) THEN

  IF ( ABS( dur_conv_rain - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for dur_conv_rain.")
  ELSE IF ( dur_conv_rain <= 0.0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "dur_conv_rain must be greater than zero.")
  END IF

  IF ( ABS( dur_ls_rain - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for dur_ls_rain.")
  ELSE IF ( dur_ls_rain <= 0.0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "dur_ls_rain must be greater than zero.")
  END IF

  IF ( ABS( dur_ls_snow - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for dur_ls_snow.")
  ELSE IF ( dur_ls_snow <= 0.0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "dur_ls_snow must be greater than zero.")
  END IF

  IF ( ABS( dur_conv_snow - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for dur_conv_snow.")
  ELSE IF ( dur_conv_snow <= 0.0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "dur_conv_snow must be greater than zero.")
  END IF

END IF  !  l_daily_disagg

!-----------------------------------------------------------------------------
! Check any necessary temperatures were provided.
!-----------------------------------------------------------------------------
IF ( io_precip_type == 1 .AND. ABS( t_for_snow - rmdi ) < EPSILON(1.0) ) THEN
  CALL log_fatal(RoutineName, "No value given for t_for_snow")
END IF

IF ( .NOT. l_point_data .AND.                                                  &
     ( io_precip_type == 1 .OR. io_precip_type == 2) ) THEN
  IF ( ABS( t_for_con_rain - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for t_for_con_rain")
  END IF
END IF

!-----------------------------------------------------------------------------
! Check temperatures for consistency.
! Note that l_point_data=T assume all precipitation is large scale and hence
! doesn't need t_for_con_rain.
!-----------------------------------------------------------------------------
IF ( .NOT. l_point_data .AND. io_precip_type == 1 ) THEN
  IF ( t_for_con_rain <= t_for_snow ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "t_for_con_rain <= t_for_snow - precip must be " //         &
                   "liquid at t_for_con_rain")
  END IF
END IF

!-----------------------------------------------------------------------------
! Check perturbations were provided and reasonable.
!-----------------------------------------------------------------------------
IF ( l_perturb_driving ) THEN

  IF ( ABS( precip_rel_perturbation - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName, "No value given for precip_rel_perturbation.")
  ELSE IF ( precip_rel_perturbation < 0.0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "precip_rel_perturbation should not be less than zero")
  END IF

  IF ( ABS(  temperature_abs_perturbation - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "No value given for temperature_abs_perturbation.")
  END IF

END IF  !  l_perturb_driving

RETURN

END SUBROUTINE init_drive
END MODULE init_drive_mod
#endif
