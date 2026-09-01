! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_irrig_mod

!-----------------------------------------------------------------------------
! Description:
!   Module containing irrigation options and a namelist for setting them
!-----------------------------------------------------------------------------


USE missing_data_mod, ONLY: imdi
USE max_dimensions, ONLY: npft_max

IMPLICIT NONE

! Parameters identifying alternative ways of calculating the irrigation
! season. There are used with variable irr_crop and should have unique values.
INTEGER, PARAMETER ::                                                          &
  irr_crop_all_year = 0,                                                       &
    ! Irrigation season lasts all year.
  irr_crop_doell = 1,                                                          &
    ! Irrigation season is determined from driving data according to Doll and
    ! Siebert (2002) method.
    ! Doell, P., and Siebert, S., 2002, Global modeling of irrigation water
    ! requirements, Water Resour. Res., 38(4).
  irr_crop_dvimax = 2
    ! Irrigation season is determined by maximum crop development index
    ! across all tiles. Requires ncpft > 0.

LOGICAL :: frac_irrig_all_tiles = .FALSE.
    ! T - assign irr fraction to all tiles
    ! F - assign irr fraction to specfied tiles only
INTEGER :: irrigtiles(npft_max) = imdi
    ! Indices of the tiles to be irrigated.
    ! Only used if frac_irrig_all_tiles = F.
    ! This is the fixed length equivalent of irrtiles where size is defined
    ! after namelist have been read in and set by irrig_vars_alloc

LOGICAL :: set_irrfrac_on_irrtiles = .FALSE.
    ! Switch to set irrigation fraction for irrigated tiles,
    ! set to FALSE to reproduce original results using frac_irrig_all_tiles

LOGICAL ::                                                                     &
  l_irrig_dmd = .FALSE.,                                                       &
    !   Switch for using irrigation demand code
  l_irrig_limit = .FALSE.,                                                     &
    !   Switch for limiting irrigation supply
  l_soil_evap_irrig_expl = .FALSE.
      ! Switch for explicitly calculating bare soil evaporation
      ! over the separate irrigated and non-irrigated fractions of
      ! the grid box. (Default uses the grid box mean soil moisture).

INTEGER :: nirrtile = imdi
    !  Number of tiles that can have irrigated fraction

INTEGER :: irr_crop = irr_crop_all_year
    !  Switch for irrigation cropping model.

INTEGER :: nstep_irrig = imdi
    ! Timestep for irrigation (number of model timesteps)

INTEGER :: irrig_option = imdi
    ! Options for applying irrigation

INTEGER, PARAMETER ::                                                          &
  no_irrigation = 0,                                                           &
    ! No irrigation
  frac_based_irrigation = 1,                                                   &
    ! Not currently available; will replace l_irrig_dmd in a future release
  tile_based_irrigation = 2
    ! Apply irrigation to available irrigated surface types

! Non-namelist variable but arrays used elsewhere after namelists are
! read in - moved out of crop_vars_mod (Hopefully this is the best place
! to put these).
INTEGER, ALLOCATABLE :: irrtiles(:)
      ! Tiles that can have irrigated fraction
      ! Only used when frac_irrig_all_tiles = .FALSE.
!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_irrig/  l_irrig_dmd, l_irrig_limit, irr_crop,                &
                          frac_irrig_all_tiles, nirrtile, irrigtiles,          &
                          set_irrfrac_on_irrtiles, nstep_irrig, irrig_option,  &
                          l_soil_evap_irrig_expl

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_IRRIG_MOD'

CONTAINS

SUBROUTINE irrig_vars_alloc(npft, l_irrig_dmd)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: npft
LOGICAL, INTENT(IN) :: l_irrig_dmd

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='IRRIG_VARS_ALLOC'
!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_irrig_dmd ) THEN
  ALLOCATE(irrtiles(npft))
  irrtiles(:) = 0
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE irrig_vars_alloc


#if !defined(RIVERS_ONLY)
SUBROUTINE check_jules_irrig()

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_message, jules_print, PrNorm
USE jules_hydrology_mod, ONLY: l_top
USE jules_vegetation_mod, ONLY: l_crop
USE jules_surface_types_mod, ONLY: c3_grass, c4_grass
!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_IRRIG namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errcode, error_sum   ! error code to pass to ereport
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_IRRIG'

#if defined(UM_JULES)
!-----------------------------------------------------------------------------
! Irrigation demand has been implemented into the UM in a limited configuration
! It will only be activated all year round (i.e. irr_crop = irr_crop_all_year)
! and only used on short vegetation (i.e. C3 and C4 grass).

IF ( l_irrig_dmd ) THEN
  errcode = -1
  CALL ereport(RoutineName, errcode,                                           &
               'Irrigation demand has been implemented into the UM' //         &
               NEW_LINE('A') //                                                &
               ' in a limited way. The configuration will be checked ' //      &
               NEW_LINE('A') //                                                &
               'and the run will terminate if a unavailable option is' //      &
               NEW_LINE('A') //                                                &
               'selected.')
  error_sum = 0
  IF ( irr_crop /=  irr_crop_all_year ) THEN
    error_sum = error_sum + 1
    WRITE(jules_message,'(I0,A,I0)') error_sum,                                &
       ": irr_crop should be 0. irr_crop = ", irr_crop
    CALL jules_print(RoutineName, jules_message, level = PrNorm)
  END IF
  IF ( set_irrfrac_on_irrtiles ) THEN
    error_sum = error_sum + 1
    WRITE(jules_message,'(I0,A,L1)') error_sum,                                &
       ": set_irrfrac_on_irrtiles should be F. " //                            &
       "set_irrfrac_on_irrtiles = ", set_irrfrac_on_irrtiles
    CALL jules_print(RoutineName, jules_message, level = PrNorm)
  END IF
  IF ( frac_irrig_all_tiles ) THEN
    error_sum = error_sum + 1
    WRITE(jules_message,'(I0,A,L1)') error_sum,                                &
       ": frac_irrig_all_tiles should be F. " //                               &
       "frac_irrig_all_tiles = ", frac_irrig_all_tiles
    CALL jules_print(RoutineName, jules_message, level = PrNorm)
  ELSE
    IF ( nirrtile /= 2 ) THEN
      error_sum = error_sum + 1
      WRITE(jules_message,'(I0,A,I0)') error_sum,                              &
         ": nirrtile should be 2. nirrtile = ", nirrtile
      CALL jules_print(RoutineName, jules_message, level = PrNorm)
    END IF
    IF ( irrigtiles(1) /= c3_grass .OR. irrigtiles(2) /= c4_grass ) THEN
      error_sum = error_sum + 1
      WRITE(jules_message,'(I0,A,2(I6))') error_sum,                           &
         ": irrigtiles(1:2) should be c3_grass=3, c4_grass=4." //              &
         NEW_LINE('A') //                                                      &
         ".. irrigtiles(1:2) = ", irrigtiles(1:2)
      CALL jules_print(RoutineName, jules_message, level = PrNorm)
    END IF
  END IF
  IF ( c3_grass /= 3 .OR. c4_grass /= 4 ) THEN
    error_sum = error_sum + 1
    WRITE(jules_message,'(I0,A,2(I6))') error_sum,                             &
       ": jules_surface_types should have c3_grass, c4_grass = 3, 4." //       &
       NEW_LINE('A') //                                                        &
       ".. c3_grass, c4_grass = ", c3_grass,  c4_grass
    CALL jules_print(RoutineName, jules_message, level = PrNorm)
  END IF
  IF ( error_sum > 0 ) THEN
    errcode = 101
    WRITE(jules_message,'(A,I0,A)') "There are ", error_sum,                   &
       " options that have been incorrectly set for " //                       &
       NEW_LINE('A') //                                                        &
       "limited UM l_irrig_dmd. Please see job output for details."
    CALL ereport(RoutineName, errcode, jules_message)
  END IF
END IF
#endif

! Check that irr_crop is reasonable.
IF ( l_irrig_dmd ) THEN
  SELECT CASE ( irr_crop )
  CASE ( irr_crop_all_year, irr_crop_doell, irr_crop_dvimax )
    ! These are valid, nothing more to do.
  CASE DEFAULT
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
               'Invalid value for irr_crop.')
  END SELECT
END IF

! Verify that if switches are on that other switches are also available
IF ( l_irrig_limit .AND. .NOT. l_irrig_dmd ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'l_irrig_limit=T requires l_irrig_dmd=T ')
END IF

IF ( l_soil_evap_irrig_expl .AND. .NOT. l_irrig_dmd ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'l_soil_evap_irrig_expl=T requires l_irrig_dmd=T ')
END IF

IF ( l_irrig_limit .AND. .NOT. l_top ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'l_irrig_limit=T requires l_top=T ')
END IF

IF ( l_irrig_dmd .AND. irr_crop ==  irr_crop_dvimax .AND. .NOT. l_crop ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               'Irrigation triggered by crop DVI (irr_crop = 2) ' //           &
               'requires crop model to be active')
END IF

IF ( set_irrfrac_on_irrtiles .AND. frac_irrig_all_tiles ) THEN
  errcode = 101
  CALL ereport(RoutineName, errcode,                                           &
               "Cannot set both frac_irrig_all_tiles and  " //                 &
               "set_irrfrac_on_irrtiles to be true ")
END IF

! irrig_option can be used when l_irrig_dmd = F
IF ( irrig_option /= imdi ) THEN
  SELECT CASE ( irrig_option )
  CASE ( no_irrigation, tile_based_irrigation )
    ! These are allowed options
  CASE ( frac_based_irrigation )
    errcode = 101
    WRITE(jules_message,'(A,I0)')                                              &
       "Fraction based irrigation is not yet available. irrig_option = ",      &
       irrig_option
    CALL ereport(RoutineName, errcode, jules_message)
  CASE DEFAULT
    errcode = 101
    WRITE(jules_message,'(A,I0)')                                              &
       "Invalid value for irrig_option. irrig_option = ", irrig_option
    CALL ereport(RoutineName, errcode, jules_message)
  END SELECT
END IF

IF ( l_irrig_dmd .AND. irrig_option /= imdi ) THEN
  errcode = 101
  WRITE(jules_message,'(A,I0)')                                                &
     "l_irrig_dmd and irrig_option cannot be used at the same time. " //       &
     "irrig_option = ", irrig_option
  CALL ereport(RoutineName, errcode, jules_message)
END IF

END SUBROUTINE check_jules_irrig
#endif

SUBROUTINE print_nlist_jules_irrig()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

!-----------------------------------------------------------------------------

CALL jules_print('jules_irrig', 'Contents of namelist jules_irrig')

WRITE(lineBuffer,*) ' l_irrig_dmd = ',l_irrig_dmd
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' l_irrig_limit = ',l_irrig_limit
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' irr_crop = ',irr_crop
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' frac_irrig_all_tiles = ',frac_irrig_all_tiles
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' nirrtile = ',nirrtile
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' irrigtiles = ',irrigtiles
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' set_irrfrac_on_irrtiles = ',set_irrfrac_on_irrtiles
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*)' nstep_irrig = ',nstep_irrig
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*)' irrig_option = ',irrig_option
CALL jules_print('jules_irrig',lineBuffer)

WRITE(lineBuffer,*) ' l_soil_evap_irrig_expl = ',l_soil_evap_irrig_expl
CALL jules_print('jules_irrig',lineBuffer)

CALL jules_print('jules_irrig',                                                &
    '- - - - - - end of namelist - - - - - -')


END SUBROUTINE print_nlist_jules_irrig

#if defined(UM_JULES) && !defined(LFRIC)

SUBROUTINE read_nml_jules_irrig (unitnumber)

! Description:
!  Read the JULES_IRRIG namelist (UM)

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_IRRIG'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_log = 4
INTEGER, PARAMETER :: n_int = 4 + npft_max

TYPE :: my_namelist
  SEQUENCE
  LOGICAL :: l_irrig_dmd
  LOGICAL :: l_irrig_limit
  LOGICAL :: frac_irrig_all_tiles
  LOGICAL :: set_irrfrac_on_irrtiles
  LOGICAL :: l_soil_evap_irrig_expl
  INTEGER :: irr_crop
  INTEGER :: nirrtile
  INTEGER :: irrigtiles(npft_max)
  INTEGER :: nstep_irrig
  INTEGER :: irrig_option
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_log_in = n_log, n_int_in = n_int)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_irrig, IOSTAT = errorstatus,            &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_irrig", iomessage)

  my_nml % l_irrig_dmd             = l_irrig_dmd
  my_nml % l_irrig_limit           = l_irrig_limit
  my_nml % frac_irrig_all_tiles    = frac_irrig_all_tiles
  my_nml % set_irrfrac_on_irrtiles = set_irrfrac_on_irrtiles
  my_nml % l_soil_evap_irrig_expl  = l_soil_evap_irrig_expl
  my_nml % irr_crop                = irr_crop
  my_nml % nirrtile                = nirrtile
  my_nml % irrigtiles              = irrigtiles
  my_nml % nstep_irrig             = nstep_irrig
  my_nml % irrig_option            = irrig_option
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  l_irrig_dmd             = my_nml % l_irrig_dmd
  l_irrig_limit           = my_nml % l_irrig_limit
  frac_irrig_all_tiles    = my_nml % frac_irrig_all_tiles
  set_irrfrac_on_irrtiles = my_nml % set_irrfrac_on_irrtiles
  l_soil_evap_irrig_expl  = my_nml % l_soil_evap_irrig_expl
  irr_crop                = my_nml % irr_crop
  nirrtile                = my_nml % nirrtile
  irrigtiles              = my_nml % irrigtiles
  nstep_irrig             = my_nml % nstep_irrig
  irrig_option            = my_nml % irrig_option
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_irrig
#endif

#if !defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_irrig(nml_dir)

! Description:
!  Read the JULES_IRRIG namelist (standalone)

USE io_constants, ONLY: max_file_name_len, namelist_unit
USE string_utils_mod, ONLY: to_string
USE logging_mod, ONLY: log_info, log_fatal
USE errormessagelength_mod, ONLY: errormessagelength
USE missing_data_mod, ONLY: imdi, rmdi


IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=max_file_name_len) :: FILE
                      ! The name of the file (or variable name template) to
                      ! use for variables that need to be filled from file

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_IRRIG'

! Open the irrigation namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_irrig.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_irrig.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
! Read namelist
CALL log_info(RoutineName, "Reading JULES_IRRIG namelist...")
READ(namelist_unit, NML = jules_irrig, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_IRRIG " //                      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file irrig.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")



END SUBROUTINE read_nml_jules_irrig
#endif

END MODULE jules_irrig_mod
