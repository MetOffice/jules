#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_imogen(nml_dir, progs_data, trifctltype, imgn_drive,           &
                       imgn_vars, ainfo)

USE io_constants, ONLY: max_file_name_len, imogen_unit, namelist_unit

USE datetime_mod, ONLY: l_360

USE string_utils_mod, ONLY: to_string

USE model_time_mod, ONLY: main_run_start, is_spinup, timesteps_in_day

USE read_dump_mod, ONLY: read_dump

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE update_mod, ONLY: l_imogen, diff_frac_const

USE ancil_info, ONLY: land_pts

USE imogen_time, ONLY: nsdmax

USE imogen_run, ONLY: imogen_run_list, c_emissions,                            &
                      include_co2, include_non_co2_radf, land_feed_co2,        &
                      land_feed_ch4, ocean_feed, nyr_emiss,                    &
                      file_scen_emits, ch4_init_ppbv, co2_init_ppmv,           &
                      initialise_from_dump, dump_file, l_change_metdata,       &
                      change_metdata_method, initial_co2_ch4_year,             &
                      l_daily_metdata_climatol, file_scen_co2_ppmv,            &
                      fch4_ref, yr_fch4_ref, tau_ch4_ref, ch4_ppbv_ref

USE imogen_anlg_vals, ONLY: imogen_anlg_vals_list, file_clim, file_patt,       &
                            diff_frac_const_imogen

USE imogen_io_vars, ONLY: nyr_max, yr_emiss, c_emiss

USE aero, ONLY: co2_mmr

USE logging_mod, ONLY: log_info, log_fatal
USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_message

USE missing_data_mod,   ONLY: rmdi, imdi
USE errormessagelength_mod, ONLY: errormessagelength

!TYPE definitions
USE prognostics, ONLY: progs_data_type
USE trifctl, ONLY: trifctl_type
USE imgn_drive_mod, ONLY: imgn_drive_type
USE imgn_vars_mod, ONLY: imgn_vars_type
USE ancil_info, ONLY: ainfo_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises IMOGEN parameters and arrays
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
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype

!TYPES containing the data
TYPE(progs_data_type), INTENT(IN OUT) :: progs_data
TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(imgn_vars_type), INTENT(IN OUT)   :: imgn_vars
TYPE(ainfo_type), INTENT(IN) :: ainfo

INTEGER, PARAMETER :: nvars = 8
INTEGER :: i ! Loop counter

CHARACTER(LEN=24), DIMENSION(nvars) ::                                         &
  imgn_name_clim = ['tl1_ij_clim_imgn        ',                                &
                    'ql1_ij_clim_imgn        ',                                &
                    'wind_ij_clim_imgn       ',                                &
                    'lwdown_ij_clim_imgn     ',                                &
                    'swdown_ij_clim_imgn     ',                                &
                    'precip_ij_clim_imgn     ',                                &
                    'pstar_ij_clim_imgn      ',                                &
                    'diurnal_tl1_ij_clim_imgn']

CHARACTER(LEN=24), DIMENSION(nvars) ::                                         &
  imgn_name_patt = ['tl1_ij_patt_imgn        ',                                &
                    'ql1_ij_patt_imgn        ',                                &
                    'wind_ij_patt_imgn       ',                                &
                    'lwdown_ij_patt_imgn     ',                                &
                    'swdown_ij_patt_imgn     ',                                &
                    'precip_ij_patt_imgn     ',                                &
                    'pstar_ij_patt_imgn      ',                                &
                    'diurnal_tl1_ij_patt_imgn']

CHARACTER(LEN=14), DIMENSION(nvars) ::                                         &
  nc_name_clim = ['tl1_clim      ',                                            &
                  'ql1_clim      ',                                            &
                  'wind_clim     ',                                            &
                  'lwdown_clim   ',                                            &
                  'swdown_clim   ',                                            &
                  'precip_clim   ',                                            &
                  'pstar_clim    ',                                            &
                  'range_tl1_clim']

CHARACTER(LEN=14), DIMENSION(nvars) ::                                         &
  nc_name_patt = ['tl1_patt      ',                                            &
                  'ql1_patt      ',                                            &
                  'wind_patt     ',                                            &
                  'lwdown_patt   ',                                            &
                  'swdown_patt   ',                                            &
                  'precip_patt   ',                                            &
                  'pstar_patt    ',                                            &
                  'range_tl1_patt']

INTEGER :: n  ! Index variables

INTEGER :: yr_co2_file
  ! Reads in years available in file "file_scen_co2_ppmv"

INTEGER :: ERROR, error_sum, errorstatus  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_IMOGEN'


!-----------------------------------------------------------------------------
! Open the IMOGEN namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'imogen.nml'),               &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file imogen.nml " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


! There are three namelists to read from this file
CALL log_info("init_imogen", "Reading IMOGEN_RUN_LIST namelist...")
READ(namelist_unit, NML = imogen_run_list, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist IMOGEN_RUN_LIST " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


CALL log_info("init_imogen", "Reading IMOGEN_ANLG_VALS_LIST namelist...")
READ(namelist_unit, NML = imogen_anlg_vals_list, IOSTAT = ERROR,               &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist IMOGEN_ANLG_VALS_LIST " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file imogen.nml " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! check all variables are set in imogen_run
!-----------------------------------------------------------------------------
! co2_init_ppmv
IF ( ABS( co2_init_ppmv - rmdi ) < EPSILON(1.0) ) THEN
  errorstatus = 101
  CALL ereport(RoutineName, errorstatus, "co2_init_ppmv not found")
ELSE IF ( co2_init_ppmv < 100.0 .OR. co2_init_ppmv > 10000.0 ) THEN
  CALL ereport(RoutineName, errorstatus,                                       &
               "co2_init_ppmv must lie in the range 100.0 to 10000.0")
END IF

! initial_co2_ch4_year
IF ( ocean_feed ) THEN
  IF ( ABS( initial_co2_ch4_year - imdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "initial_co2_ch4_year not found")
  ELSE IF ( initial_co2_ch4_year < 1000 .OR. initial_co2_ch4_year > 5000 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 "initial_co2_ch4_year must lie in the range 1000 to 5000")
  END IF
END IF

! change_metdata_method
IF ( l_change_metdata ) THEN
  IF ( ABS( change_metdata_method - imdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "change_metdata_method not found")
  END IF
END IF

! yr_fch4_ref, ch4_init_ppbv, fch4_ref, tau_ch4_ref, ch4_ppbv_ref
IF ( land_feed_ch4 ) THEN
  IF ( ABS( yr_fch4_ref - imdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "yr_fch4_ref not found")
  ELSE IF ( yr_fch4_ref < 1000 .OR. yr_fch4_ref > 5000 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 "yr_fch4_ref must lie in the range 1000 to 5000")
  END IF

  IF ( ABS( ch4_init_ppbv - rmdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "ch4_init_ppbv not found")
  ELSE IF ( ch4_init_ppbv < 100.0 .OR. ch4_init_ppbv > 5000.0 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 "ch4_init_ppbv must lie in the range 100.0 to 5000.")
  END IF

  IF ( ABS( fch4_ref - rmdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "fch4_ref not found")
  ELSE IF ( fch4_ref < 1.0 .OR. fch4_ref > 1000.0 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 "fch4_ref must lie in the range 1.0 to 1000.")
  END IF

  IF ( ABS( tau_ch4_ref - rmdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "tau_ch4_ref not found")
  ELSE IF ( tau_ch4_ref < 0.1 .OR. tau_ch4_ref > 100.0 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 "tau_ch4_ref must lie in the range 0.1 to 100.0")
  END IF

  IF ( ABS( ch4_ppbv_ref - rmdi ) < EPSILON(1.0) ) THEN
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, "ch4_ppbv_ref not found")
  ELSE IF ( ch4_ppbv_ref < 100.0 .OR. ch4_ppbv_ref > 5000.0 ) THEN
    CALL ereport(RoutineName, errorstatus,                                     &
                 "ch4_ppbv_ref must lie in the range 100.0 to 5000.")
  END IF

END IF

!-----------------------------------------------------------------------------
! Check that the setup of JULES time variables is compatible with IMOGEN and
! set up IMOGEN time variables
!-----------------------------------------------------------------------------
! Force 360 day year
IF ( .NOT. l_360 )                                                             &
  CALL log_fatal(RoutineName, "360 day year must be used with IMOGEN")

! Run must start at 00:00 on 1st Jan for some year
IF ( main_run_start%TIME /= 0 .OR. main_run_start%day /= 1 .OR.                &
                                   main_run_start%month /= 1 )                 &
  CALL log_fatal(RoutineName,                                                  &
                 "IMOGEN runs must start at 00:00 on 1st Jan for some year")

! Set steps per day and check it is not more than the maximum number of
! timesteps per day allowed by IMOGEN
IF (timesteps_in_day > nsdmax)                                                 &
  CALL log_fatal(RoutineName, "Too many timesteps per day")


!-----------------------------------------------------------------------------
! Check that the configurations proposed are valid.
! This routine also checks whether the selected configuration has been
! confirmed. Unconfirmed configurations return a WARNING message, but do not
! break the run.
!-----------------------------------------------------------------------------
CALL imogen_check(                                                             &
  c_emissions, include_co2, include_non_co2_radf, land_feed_co2,               &
  land_feed_ch4, ocean_feed, change_metdata_method, l_change_metdata)

!-----------------------------------------------------------------------------
! Now get the emissions/CO2 concentrations.
! At present only coded for reading in a file of emission/CO2 concentration
! imogen projects of the future climate, with carbon cycle feedbacks) or
! in a file of CO2 concentrations for "Hydrology 20th Century" simulation
!-----------------------------------------------------------------------------
IF (nyr_emiss > nyr_max) THEN
  CALL log_fatal(RoutineName, "Time series of emissions is " //                &
                 "longer than allocated array"               //                &
                 "check nyr_emiss and nyr_max")
END IF

IF ( c_emissions .AND. include_co2 .AND. l_change_metdata                      &
     .AND. change_metdata_method == 1 ) THEN
  OPEN(imogen_unit, FILE=file_scen_emits,                                      &
          STATUS='old', POSITION='rewind', ACTION='read', IOSTAT=ERROR)
  IF (ERROR /= 0) THEN
    CALL log_fatal(RoutineName, ': file_scen_emits not found')
  END IF

  DO n = 1,nyr_emiss
    READ(imogen_unit,*, IOSTAT=ERROR) yr_emiss(n),c_emiss(n)
    IF (ERROR /= 0) THEN
      CALL log_fatal(RoutineName, ': error reading file_scen_emits')
    END IF
  END DO

  CLOSE(imogen_unit)
END IF

IF ( include_co2) THEN
  OPEN(imogen_unit, FILE=file_scen_co2_ppmv,                                   &
            STATUS='old', POSITION='rewind', ACTION='read', IOSTAT=ERROR)
  IF (ERROR /= 0) THEN
    CALL log_fatal(RoutineName, ': file_scen_co2_ppmv not found')
  END IF

  DO WHILE ( .TRUE. )
    READ(imogen_unit,FMT=*,IOSTAT = ERROR) yr_co2_file, co2_init_ppmv
    IF (ERROR /= 0) THEN
      CALL log_fatal(RoutineName, ': error reading file_scen_co2_ppmv')
    END IF
    ! ejb debug PRINT*,yr_co2_file, initial_co2_ch4_year  !ejb ceck this is non zero

    IF (yr_co2_file == initial_co2_ch4_year) THEN
      imgn_vars%co2_ppmv(:) = co2_init_ppmv
      EXIT
    END IF
  END DO

ELSE
  CALL log_fatal(RoutineName, ': include_co2 is false and tryimg to read co2 from file')
END IF


!-----------------------------------------------------------------------
! Read in monthly climatology of climate data.
!-----------------------------------------------------------------------
! In the following call, is_climatology adds a 12-month time dimension to the
! size of the input variable in order to interpolate to the appropriate start
! time for initial conditions. It is therefore .FALSE. in these calls.
CALL fill_variables_from_file( file_clim, imgn_name_clim, nc_name_clim,        &
                               is_climatology = [ (.FALSE., i=1, nvars) ] )

IF ( (l_change_metdata) .AND.                                                  &
       (change_metdata_method ==1 .OR. change_metdata_method ==3) ) THEN
  !-----------------------------------------------------------------------
  ! Read in spatial climate patterns (/K) - only read in once
  ! there is a value for every grid cell and every month
  ! patterns used for the driving climate data for the following cases:
  !    - driving directly with global temperatures
  !      (l_change_metdata=.TRUE. and change_metdata_method=3)
  !    - anomalies prescribed from patterns using the analogue model where
  !      the global temperature change is calculated from the change in
  !      radiative forcing (l_change_metdata=.TRUE. and change_metdata_method=1)
  !-----------------------------------------------------------------------
  CALL fill_variables_from_file( file_patt, imgn_name_patt, nc_name_patt,      &
                                 is_climatology = [ (.FALSE., i=1, nvars) ] )
END IF

!-----------------------------------------------------------------------------
! Set up the initial conditions and intialise values
!-----------------------------------------------------------------------------
IF (land_feed_ch4) THEN
  imgn_vars%ch4_ppbv = ch4_init_ppbv
END IF

! deleted imgn_vars%co2_ppmv(:) = co2_init_ppmv

IF ( include_co2 ) imgn_vars%co2_change_ppmv(:) = 0.0

IF (l_change_metdata .AND. change_metdata_method ==1 ) THEN
  imgn_vars%dtemp_o(:) = 0.0   ! anlg_model

  IF ( include_co2 .AND. ocean_feed .AND. c_emissions ) THEN
    imgn_vars%fa_ocean(:)=0.0
    ! already initialsed in allocation
  END IF
END IF

! In IMOGEN, the seed has 4 components
ALLOCATE( progs_data%seed_rain(4) )

! Initiate seeding values for subdaily rainfall
progs_data%seed_rain(1) = 9465
progs_data%seed_rain(2) = 1484
progs_data%seed_rain(3) = 3358
progs_data%seed_rain(4) = 8350

trifctltype%cv_gb(:) = 0.0

! Override the IMOGEN variables from given dump file if we have been asked to
IF ( initialise_from_dump ) THEN
  CALL read_dump(dump_file, [ 'co2_ppmv       ', 'co2_change_ppmv',            &
                               'dtemp_o        ', 'fa_ocean       ',           &
                               'seed_rain      ', 'cv             ' ])

  IF (land_feed_ch4) THEN
    CALL read_dump(dump_file, [ 'ch4_ppbv' ] )
  END IF
END IF

! Unit conversion ppm to mmr (g/g): mol mass co2 / mol mass dry air * 1e-6
co2_mmr = imgn_vars%co2_ppmv(1) * 44.0 / 28.97 * 1.0e-6

!-------------------------------------------------------------------------------
! Set diff_frac_const from IMOGEN input
!-------------------------------------------------------------------------------
! Metadata does not allow "OR" triggering so in order to preserve the original
! diff_frac_const functionality it is necessary for IMOGEN to have its own value
diff_frac_const = diff_frac_const_imogen

RETURN

END SUBROUTINE init_imogen
#endif
