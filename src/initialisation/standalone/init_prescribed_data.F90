#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_prescribed_data(nml_dir)

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, max_var_file,     &
                         namelist_unit, file_list_unit

USE missing_data_mod, ONLY: imdi

USE model_interface_mod, ONLY: identifier_len

USE datetime_mod, ONLY: datetime_str_len, datetime, datetime_from_string

USE string_utils_mod, ONLY: to_string

USE max_dimensions, ONLY: sm_levels_max

USE templating_mod, ONLY: tpl_detect_period, tpl_has_var_name,                 &
                           tpl_substitute_var

USE time_varying_input_mod, ONLY: register_input_file

USE update_mod, ONLY: have_prescribed_veg, have_prescribed_sthuf,              &
                       prescribed_sthuf_levels, l_imogen

USE jules_deposition_mod, ONLY: l_deposition, l_deposition_flux

USE jules_radiation_mod, ONLY: l_albedo_obs, l_spec_albedo

USE jules_rivers_mod, ONLY: l_vary_sea_level, rivers_regrid

USE jules_vegetation_mod, ONLY: l_triffid, l_phenol, l_o3_damage, l_crop,      &
                                l_croprotate

USE jules_irrig_mod, ONLY: l_irrig_dmd

USE jules_water_resources_mod, ONLY: l_water_domestic, l_water_industry,       &
    l_water_livestock, l_water_resources, l_water_transfers

USE logging_mod, ONLY: log_info, log_error, log_fatal, log_warn

USE errormessagelength_mod, ONLY: errormessagelength

USE jules_hydrology_mod, ONLY: l_top

USE jules_soil_mod, ONLY: sm_levels

USE imogen_run, ONLY: l_change_metdata, change_metdata_method

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about what variables are prescribed on what timestep
!   from what files and initialises the files
!   We allow the user to specify any variables they like, so long as the code
!   exists in model_interface_mod to populate them from file
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

! Work variables
TYPE(datetime) :: data_start_dt, data_end_dt
                       ! Datetime objects created from strings given in
                       ! output profiles

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

INTEGER :: nvars_total  ! The total number of prescribed variables across all
                        ! datasets
CHARACTER(LEN=identifier_len), ALLOCATABLE :: all_vars(:)
                               ! List of all variable identifiers from all
                               ! datasets

INTEGER :: i,j,k  ! Index variables

INTEGER :: ERROR, error_sum  ! Error indicators

!-----------------------------------------------------------------------------
! Definition of the jules_prescribed namelist
!-----------------------------------------------------------------------------
INTEGER :: n_datasets      ! The number of datasets containing prescribed
                           ! variables
NAMELIST  / jules_prescribed/ n_datasets

!-----------------------------------------------------------------------------
! Definition of the jules_prescribed_dataset namelist
!-----------------------------------------------------------------------------
! Information about the data in the file
CHARACTER(LEN=datetime_str_len) :: data_start, data_end
                           ! Start and end times for data as strings
INTEGER :: data_period     ! The period of the driving data
LOGICAL :: is_climatology  ! T - use the file to provide a climatology
                           !     for the specified variables
                           ! F - the file is not used as a climatology

! Information about the files to use
LOGICAL :: read_list       ! T - the given file contains a list of file
                           !     names and times of first data
                           !     These files may contain variable name
                           !     templating but not time templating
                           ! F - data should be read directly from
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
INTEGER :: prescribed_levels(sm_levels_max)
                      ! Subset of levels to be prescribed.
                      ! Length sm_levels_max rather than sm_levels
                      ! so that if this is set for a variable that's not
                      ! sthuf, there's more chance of getting far enough
                      ! for this to be explicitly checked in the code, and a more
                      ! informative error message given.
INTEGER :: n_presc_levels
                      ! number of prescribed levels
CHARACTER(LEN=identifier_len) :: var(max_var_file)
                      ! The variable identifiers of the variables
CHARACTER(LEN=max_sdf_name_len) :: var_name(max_var_file)
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(max_var_file)
                      ! The name to substitute in a template for each
                      ! variable
CHARACTER(LEN=2) :: interp(max_var_file)
                      ! Flag indicating the type of interpolation to use
                      ! for each variable
CHARACTER(LEN=errormessagelength) :: iomessage

NAMELIST  / jules_prescribed_dataset/ data_start, data_end, data_period,       &
                                    is_climatology, read_list, nfiles, FILE,   &
                                    nvars, var, var_name, tpl_name, interp,    &
                                    prescribed_levels


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
n_datasets = 0

nvars_total = 0

!-----------------------------------------------------------------------------
! Read namelists
!-----------------------------------------------------------------------------
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'prescribed_data.nml'),      &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_prescribed_data",                                       &
                 "Error opening namelist file prescribed_data.nml " //         &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Read the JULES_PRESCRIBED namelist
!-----------------------------------------------------------------------------
CALL log_info("init_prescribed_data", "Reading JULES_PRESCRIBED namelist...")
READ(namelist_unit, NML = jules_prescribed, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_prescribed_data",                                       &
                 "Error reading namelist JULES_PRESCRIBED " //                 &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Process the information from the JULES_PRESCRIBED namelist
!-----------------------------------------------------------------------------
IF ( n_datasets < 1 ) THEN
  CALL log_info("init_prescribed_data",                                        &
                "No data (other than driving data) is prescribed for " //      &
                "this run")
END IF

! Now we know how many datasets there are, we know the maximum possible size
! for the all_vars array
ALLOCATE(all_vars(max_var_file * n_datasets))
all_vars(:) = ''

!-----------------------------------------------------------------------------
! Read and process information about each input file in turn
!-----------------------------------------------------------------------------
DO i = 1,n_datasets
  !-----------------------------------------------------------------------------
  ! Set namelist values to their defaults before reading about the next file
  !-----------------------------------------------------------------------------
  data_start           = ''
  data_end             = ''
  data_period          = imdi
  is_climatology       = .FALSE.
  read_list            = .FALSE.
  nfiles               = 0
  nvars                = 0
  FILE=''
  var(:)               = ''       ! Empty identifiers.
  var_name(:)          = ''       ! Empty variable names.
  tpl_name(:)          = ''       ! Empty template name.
  interp(:)            = ''       ! Empty interpolation flag.
  prescribed_levels(:) = imdi

  !-----------------------------------------------------------------------------
  ! Read the namelist
  !-----------------------------------------------------------------------------
  CALL log_info("init_prescribed_data",                                        &
                "Reading JULES_PRESCRIBED_DATASET namelist...")
  READ(namelist_unit, NML = jules_prescribed_dataset, IOSTAT = ERROR,          &
       IOMSG = iomessage)
  IF ( ERROR /= 0 )                                                            &
    CALL log_fatal("init_prescribed_data",                                     &
                   "Error reading namelist JULES_PRESCRIBED_DATASET " //       &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")

  !-----------------------------------------------------------------------------
  ! If the file is not providing any variables, we can skip it
  !-----------------------------------------------------------------------------
  IF ( nvars < 1 ) THEN
    CALL log_error("init_prescribed_data",                                     &
                   "File " // TRIM(FILE) // " is not providing any " //        &
                   "variables - ignoring")
    CYCLE
  END IF

  !-----------------------------------------------------------------------------
  ! Convert data_start and data_end to datetime objects
  !-----------------------------------------------------------------------------
  !   Check that the dates have been given.
  IF ( LEN_TRIM(data_start) == 0 .OR. LEN_TRIM(data_end) == 0 )                &
    CALL log_fatal("init_prescribed_data",                                     &
                   "data_start and data_end must both be specified")
  data_start_dt = datetime_from_string(data_start)
  data_end_dt   = datetime_from_string(data_end)

  !-----------------------------------------------------------------------------
  ! Check data_period was given.
  !-----------------------------------------------------------------------------
  IF ( data_period == imdi )                                                   &
    CALL log_fatal("init_prescribed_data",                                     &
                   "data_period must be specified")

  !-----------------------------------------------------------------------------
  ! Check that a file name was provided.
  !-----------------------------------------------------------------------------
  IF ( LEN_TRIM(FILE) == 0 )                                                   &
    CALL log_fatal("init_prescribed_data", "No file name provided")

  !-----------------------------------------------------------------------------
  ! Check that variable identifiers are not empty.
  !-----------------------------------------------------------------------------
  DO j = 1,nvars
    IF ( LEN_TRIM(var(j)) == 0 )                                               &
      CALL log_fatal("init_prescribed_data",                                   &
                     "Insufficient values for var. " //                        &
                     "No name provided for var at position #" //               &
                     TRIM(to_string(j)) )
  END DO

  !-----------------------------------------------------------------------------
  ! Check that interpolation flags are not empty.
  !-----------------------------------------------------------------------------
  DO j = 1,nvars
    IF ( LEN_TRIM(interp(j)) == 0 )                                            &
      CALL log_fatal("init_prescribed_data",                                   &
                   "No value given for interp (interpolation flag) for " //    &
                   "variable " // TRIM(var(j)) )
  END DO

  !-----------------------------------------------------------------------------
  ! Work out what files we will be using to read driving data
  !-----------------------------------------------------------------------------
  IF ( read_list ) THEN
    IF ( nfiles < 1 )                                                          &
      CALL log_fatal("init_prescribed_data",                                   &
                     "If reading a list of file names and file times, " //     &
                     "at least one file must be given")

    ! If we are reading a list of files, then we will definitely not be using
    ! time templating
    use_time_tpl = .FALSE.

    !-----------------------------------------------------------------------------
    ! Read the list of file names and times from the file
    !-----------------------------------------------------------------------------
    CALL log_info("init_prescribed_data",                                      &
                  "Reading list of data file names and start times " //        &
                  "from " // TRIM(FILE) // "...")

    ! First allocate space for the elements
    ALLOCATE(file_names(nfiles), STAT = ERROR)
    error_sum = ERROR
    ALLOCATE(file_times(nfiles), STAT = ERROR)
    error_sum = error_sum + ERROR
    IF ( error_sum /= 0 )                                                      &
      CALL log_fatal("init_prescribed_data",                                   &
                     "Error allocating arrays for file names and times")

    ! Open the file
    OPEN(file_list_unit, FILE=FILE, STATUS='old', POSITION='rewind',           &
                                               ACTION='read', IOSTAT = ERROR,  &
                                               IOMSG = iomessage)
    IF ( ERROR /= 0 )                                                          &
      CALL log_fatal("init_prescribed_data",                                   &
                     "Error opening file " // TRIM(FILE) // " " //             &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")

    ! Read the number of files that we have been told to read
    DO j = 1,nfiles
      READ(file_list_unit, *, IOSTAT = ERROR, IOMSG = iomessage) file_names(j),&
                                                             file_time_str
      IF ( ERROR /= 0 )                                                        &
        CALL log_fatal("init_prescribed_data",                                 &
                       "Error reading file name/time pair from file " //       &
                       "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //    &
                       TRIM(iomessage) // ")")
      ! Convert the file time as a string into a datetime object
      file_times(j) = datetime_from_string(file_time_str)
    END DO

    ! Close the file
    CLOSE(file_list_unit, IOSTAT = ERROR, IOMSG = iomessage)
    IF ( ERROR /= 0 )                                                          &
      CALL log_fatal("init_prescribed_data",                                   &
                     "Error closing file " // TRIM(FILE) // " " //             &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")
  ELSE
    !-----------------------------------------------------------------------------
    ! If we are not using a list of files, then we detect if we are using time
    ! templating or not
    !-----------------------------------------------------------------------------
    use_time_tpl = ( tpl_detect_period(FILE) < 0 )

    IF ( use_time_tpl ) THEN
      CALL log_info("init_prescribed_data",                                    &
                    "Using time templating to get data file names")
    ELSE
      CALL log_info("init_prescribed_data",                                    &
                    "Using single file for all data times")
    END IF

    !-----------------------------------------------------------------------------
    ! If we are not using templating, then we give lists indicating one file that
    ! starts at the data start time to register_input_file
    !-----------------------------------------------------------------------------
    ALLOCATE(file_names(1), STAT = ERROR)
    error_sum = ERROR
    ALLOCATE(file_times(1), STAT = ERROR)
    error_sum = error_sum + ERROR
    IF ( error_sum /= 0 )                                                      &
      CALL log_fatal("init_prescribed_data",                                   &
                     "Error allocating arrays for file names and times")

    file_names(1) = FILE
    file_times(1) = data_start_dt
  END IF

  !-----------------------------------------------------------------------------
  ! Check if we are going to be using variable name templating or not
  !-----------------------------------------------------------------------------
  IF ( use_time_tpl ) THEN
    ! If using time templating, just check the given file name for
    ! variable templating
    use_var_name_tpl = tpl_has_var_name(FILE)
  ELSE
    ! If using a list of files, check that they either all have variable templating
    ! or all don't

    ! Check the first file first
    use_var_name_tpl = tpl_has_var_name(file_names(1))

    DO j = 2,nfiles
      ! Check that the rest of the file names match
      IF ( use_var_name_tpl .NEQV. tpl_has_var_name(file_names(j)) )           &
        CALL log_fatal("init_prescribed_data",                                 &
                       "If providing a list of files, either they must " //    &
                       "all use variable name templating or all NOT use " //   &
                       "variable name templating")
    END DO
  END IF

  IF ( use_var_name_tpl )                                                      &
    CALL log_info("init_prescribed_data",                                      &
                  "Using variable name templating to get data file names")

  !-----------------------------------------------------------------------------
  ! Register the input file(s)
  !-----------------------------------------------------------------------------
  IF ( use_var_name_tpl ) THEN
    ! If we are using a variable name template, we must loop through the variables
    ! and register one file (set of files) per variable
    DO j = 1,nvars
      ! Check that a template string was provided for this variable.
      IF ( LEN_TRIM(tpl_name(j)) == 0 )                                        &
        CALL log_fatal( "init_prescribed_data",                                &
                        "No variable name template substitution " //           &
                        "provided for " // TRIM(var(j)) )
      CALL register_input_file(data_start_dt, data_end_dt, data_period,        &
                               is_climatology, use_time_tpl,                   &
      ! Substitute the variable name into the file
                                       tpl_substitute_var(FILE, tpl_name(j)),  &
      ! Build the list of file names using an array comprehension by substituting
      ! the variable name into each file name
                                       [ (tpl_substitute_var(                  &
                                             file_names(k), tpl_name(j)        &
                                       ), k = 1,SIZE(file_names)) ],           &
                                       file_times,                             &
      ! Use array constructors to give the single values related to the variable
                                       [ var(j) ], [ var_name(j) ],            &
                                       [ interp(j) ])
    END DO

  ELSE
    ! We are not using variable name templating, so register the same file as
    ! providing all variables
    CALL register_input_file(data_start_dt, data_end_dt, data_period,          &
                             is_climatology, use_time_tpl,                     &
                             FILE, file_names, file_times,                     &
                             var(1:nvars), var_name(1:nvars),                  &
                             interp(1:nvars))
  END IF

  DEALLOCATE(file_names)
  DEALLOCATE(file_times)

  ! Accumulate the variables provided by this dataset
  all_vars(nvars_total+1:nvars_total + nvars) = var(1:nvars)
  nvars_total = nvars_total + nvars

  !-----------------------------------------------------------------------------
  ! Check whether prescribing a subset of levels only
  !-----------------------------------------------------------------------------

  IF ( ANY( prescribed_levels /= imdi ) ) THEN
    IF ( ( nvars == 1 ) .AND. ( var(1) == 'sthuf' ) ) THEN
      n_presc_levels = 0
      DO j = 1, SIZE(prescribed_levels)
        IF ( prescribed_levels(j) /= imdi ) THEN
          n_presc_levels = n_presc_levels + 1
        END IF
      END DO
      ALLOCATE(prescribed_sthuf_levels(n_presc_levels))
      prescribed_sthuf_levels(1:n_presc_levels) =                              &
                                         prescribed_levels(1:n_presc_levels)
      IF ( ( MAXVAL(prescribed_sthuf_levels) > sm_levels ) .OR.                &
           ( MINVAL(prescribed_sthuf_levels) <= 0 ) ) THEN
        CALL log_fatal("init_prescribed_data",                                 &
                       "Prescribed levels should be in the range" //           &
                       "1 to sm_levels.")
      END IF
    ELSE
      CALL log_fatal("init_prescribed_data",                                   &
                     "Can currently only prescribe a subset of levels " //     &
                     "for nvars=1 and var=sthuf.")
    END IF
  ELSE IF ( ANY( var(1:nvars) == 'sthuf' ) ) THEN
    n_presc_levels = sm_levels
    ALLOCATE(prescribed_sthuf_levels(n_presc_levels))
    DO j = 1, sm_levels
      prescribed_sthuf_levels(j) = j
    END DO
  END IF

  !-----------------------------------------------------------------------------
  ! Report the variables that are prescribed.
  !-----------------------------------------------------------------------------
  DO j = 1,nvars
    CALL log_info("init_prescribed_data", "variable: " // TRIM(var(j)) )
  END DO

END DO  ! datasets

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_prescribed_data",                                       &
                 "Error closing namelist file prescribed_data.nml " //         &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Constraints on imogen to make sure all variables are read in
!-----------------------------------------------------------------------------
IF ( l_imogen ) THEN
  IF ( l_change_metdata ) THEN

    !---------------------------------------------------------------------------
    ! GLOBAL MEAN TEMPERATURE DRIVEN RUN - no feedbacks here yet
    ! need dtemp_g
    IF ( change_metdata_method == 3 ) THEN
      IF ( .NOT. ANY(all_vars == 'dtemp_g') ) THEN
        CALL log_fatal("init_prescribed_data",                                 &
                      "When using IMOGEN driven by global temp change " //     &
                      "need to prescribe dtemp_g")
      END IF
    END IF

  END IF
END IF

!-----------------------------------------------------------------------------
! Can't have ozone damage without prescribing ozone
!-----------------------------------------------------------------------------
IF ( l_o3_damage .AND. .NOT. ANY(all_vars == 'ozone') )                        &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When ozone damage is on, ozone must be prescribed")

!-----------------------------------------------------------------------------
! Can't calculate deposition fluxes without prescribing surface
! concentrations. Note there is potential overlap with the prescription of
! ozone for l_o3_damage - to be resolved in future.
!-----------------------------------------------------------------------------
IF ( l_deposition_flux .AND. .NOT. ANY(all_vars == 'deposition_conc') ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "When deposition fluxes are required, " //                    &
                 "surface concentrations of deposited species " //             &
                 "must be prescribed.")
END IF

!-----------------------------------------------------------------------------
! Don't allow boundary layer height to be prescribed unless deposition is
! selected. There's not necessarily any harm in allowing the height to be
! prescribed, but it could cause confusion if it went unnoticed!
!-----------------------------------------------------------------------------
IF ( .NOT. l_deposition .AND. ANY(all_vars == 'bl_height') ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "Boundary layer height should only be prescribed if " //      &
                 "deposition is selected.")
END IF

!-----------------------------------------------------------------------------
! If we are scaling albedo to agree with obs, then we need to make sure we
! have the appropriate obs
!-----------------------------------------------------------------------------
IF ( l_albedo_obs ) THEN
  IF ( l_spec_albedo ) THEN
    IF ( .NOT. ANY(all_vars == 'albobs_vis') .OR.                              &
         .NOT. ANY(all_vars == 'albobs_nir') )                                 &
      CALL log_fatal("init_prescribed_data",                                   &
                     "When l_albedo_obs = T and l_spec_albedo = T, " //        &
                     "albobs_vis and albobs_nir must be prescribed")
  ELSE ! l_spec_albedo
    IF ( .NOT. ANY(all_vars == 'albobs_sw') )                                  &
      CALL log_fatal("init_prescribed_data",                                   &
                     "When l_albedo_obs = T and l_spec_albedo = F, " //        &
                     "albobs_sw must be prescribed")
  END IF
END IF


!-----------------------------------------------------------------------------
! Can't prescribe lai if phenology is on, since it is prognostic
!-----------------------------------------------------------------------------
IF ( l_phenol .AND. ANY(all_vars == 'lai') )                                   &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When phenology is on, lai is prognostic and cannot " //      &
                 "be prescribed")


!-----------------------------------------------------------------------------
! Can't prescribe canopy height if TRIFFID is on, since it is prognostic
!-----------------------------------------------------------------------------
IF ( l_triffid .AND. ANY(all_vars == 'canht') )                                &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When TRIFFID is on, canopy height is prognostic and " //     &
                 "cannot be prescribed")

IF ( l_triffid .AND. ANY(all_vars == 'lai') )                                  &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When TRIFFID is on, lai cannot be prescribed")

IF ( l_crop .AND. ANY(all_vars == 'canht') )                                   &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When l_crop=T, canopy height is prognostic and " //          &
                 "cannot be prescribed")

IF ( l_crop .AND. ANY(all_vars == 'lai') )                                     &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When l_crop=T, lai is prognostic and " //                    &
                 "cannot be prescribed")

!-----------------------------------------------------------------------------
! l_croprotate checks
!-----------------------------------------------------------------------------

IF ( l_croprotate .AND. .NOT. ANY(all_vars == 'cropfrac') )                    &
   CALL log_fatal("init_prescribed_data",                                      &
                  "When l_croprotate=T, cropfrac needs to be prescribed.")

!-----------------------------------------------------------------------------
! Check for CaMa-Flood with a time-varying sea level.
!-----------------------------------------------------------------------------
IF ( l_vary_sea_level .AND. .NOT. ANY(all_vars == 'sea_level_lp') ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "When sea level at river mouths is required  " //             &
                 "(l_vary_sea_level=T), sea_level_lp must be prescribed.")
END IF

!-----------------------------------------------------------------------------
! Water resource variables.
!-----------------------------------------------------------------------------
! We must prescribe a demand if a particular water sector is active, and we
! must not prescribe a demand for an inactive sector or if water resources
! are not being modelled (as in both cases space has not been allocated).
! Note that the sector-specific flags (e.g. l_water_domestic) have been set
! to F if l_water_resources=F.
! Irrigation and environmental demands cannot be prescribed.

! Domestic
IF ( l_water_domestic ) THEN
  IF ( .NOT. ANY( all_vars == 'demand_rate_domestic' ) ) THEN
    CALL log_fatal("init_prescribed_data",                                     &
                   "l_water_domestic=T requires that " //                      &
                   "demand_rate_domestic is prescribed.")
  END IF
ELSE IF ( ANY( all_vars == 'demand_rate_domestic' ) ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "l_water_domestic=F requires that " //                        &
                 "demand_rate_domestic is NOT prescribed.")
END IF

! Industry.
IF ( l_water_industry ) THEN
  IF ( .NOT. ANY( all_vars == 'demand_rate_industry' ) ) THEN
    CALL log_fatal("init_prescribed_data",                                     &
                   "l_water_industry=T requires that " //                      &
                   "demand_rate_industry is prescribed.")
  END IF
ELSE IF ( ANY( all_vars == 'demand_rate_industry' ) ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "l_water_industry=F requires that " //                        &
                 "demand_rate_industry is NOT prescribed.")
END IF

! Livestock
IF ( l_water_livestock ) THEN
  IF ( .NOT. ANY( all_vars == 'demand_rate_livestock' ) ) THEN
    CALL log_fatal("init_prescribed_data",                                     &
                   "l_water_livestock=T requires that " //                     &
                   "demand_rate_livestock is prescribed.")
  END IF
ELSE IF ( ANY( all_vars == 'demand_rate_livestock' ) ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "l_water_livestock=F requires that " //                       &
                 "demand_rate_livestock is NOT prescribed.")
END IF

! Transfers
IF ( l_water_transfers ) THEN
  IF ( .NOT. ANY( all_vars == 'demand_rate_transfers' ) ) THEN
    CALL log_fatal("init_prescribed_data",                                     &
                   "l_water_transfers=T requires that " //                     &
                   "demand_rate_transfers is prescribed.")
  END IF
ELSE IF ( ANY( all_vars == 'demand_rate_transfers' ) ) THEN
  CALL log_fatal("init_prescribed_data",                                       &
                 "l_water_transfers=F requires that " //                       &
                 "demand_rate_transfers is NOT prescribed.")
END IF

!-----------------------------------------------------------------------------
! Check if any of the specified variables require special treatment, either
! here or in update_derived_variables (once the prescribed variables
! themselves have been updated from file).
!-----------------------------------------------------------------------------
! update_mod needs to know if we have a veg variable or sthuf prescribed
!-----------------------------------------------------------------------------
have_prescribed_veg = ANY(all_vars == 'lai') .OR. ANY(all_vars == 'canht')

have_prescribed_sthuf = ANY(all_vars == 'sthuf')

IF ( have_prescribed_sthuf .AND. l_top )                                       &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When l_top=T, sthuf cannot be prescribed")

IF ( have_prescribed_sthuf .AND. l_irrig_dmd )                                 &
  CALL log_fatal("init_prescribed_data",                                       &
                 "When l_irrig_dmd=T, sthuf cannot be prescribed")

DEALLOCATE(all_vars)

!------------------------------------------------------------------------------
! Warn if a time-varying sea level is used with rivers_regrid.
! This assumes that init_rivers_props has been called earlier.
!------------------------------------------------------------------------------
IF ( l_vary_sea_level .AND. rivers_regrid ) THEN
  CALL log_warn("init_prescribed_data",                                        &
                "When speciying sea level at river mouths "                 // &
                "(l_vary_sea_level=T) and regridding river fields "         // &
                "(rivers_regrid=T) care must be taken to ensure that "      // &
                "correct values result at all river mounths.")
END IF

RETURN

END SUBROUTINE init_prescribed_data
#endif
