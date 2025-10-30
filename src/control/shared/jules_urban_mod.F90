! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_urban_mod

! -----------------------------------------------------------------------------
! Description:
!   Contains switches and other inputs for the urban scheme MORUSES.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
! -----------------------------------------------------------------------------

USE max_dimensions,    ONLY: nsurft_max
USE um_types,          ONLY: real_jlslsm
USE missing_data_mod,  ONLY: rmdi

IMPLICIT NONE

REAL(KIND=real_jlslsm) ::                                                      &
   anthrop_heat_scale = rmdi ! Scales anthropogenic heat source of roof
                             ! canyon from being equally distributed (= 1.0)
                             ! to all being released in the canyon (= 0.0).
                             ! Takes a value between 0.0 - 1.0

LOGICAL ::                                                                     &
     l_urban_empirical      = .FALSE.,  & ! Empirical relationships for urban
                                          ! geometry (WRR, HWR & HGT)
                                          ! (Standalone only)
     l_moruses_macdonald    = .FALSE.,  & ! MacDonald formulation for
                                          ! displacement height and effective
                                          ! roughness length for momentum
     l_moruses              = .FALSE.,  & ! MORUSES umbrella switch

! Independent parametristaion switches
     l_moruses_albedo       = .FALSE.,  & ! SW canyon albedo
     l_moruses_emissivity   = .FALSE.,  & ! LW canyon emissivity
     l_moruses_rough        = .FALSE.,  & ! Heat transfer
     l_moruses_storage      = .FALSE.,  & ! Storage
     l_moruses_storage_thin = .FALSE.,  & ! Storage thin roof

     l_moruses_rough_surft(nsurft_max)  = .FALSE.,                             &
     l_moruses_albedo_surft(nsurft_max) = .FALSE.

!-----------------------------------------------------------------------
! Set up a namelist to allow switches to be set. l_moruses is set by
! inspecting all other moruses parametrisations.
!-----------------------------------------------------------------------
NAMELIST  /jules_urban/                                                        &
   anthrop_heat_scale, l_urban_empirical,                                      &
   l_moruses_albedo,l_moruses_emissivity,l_moruses_rough,                      &
   l_moruses_storage,l_moruses_storage_thin,l_moruses_macdonald

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_URBAN_MOD'

CONTAINS

#if !defined(RIVERS_ONLY)
SUBROUTINE check_jules_urban()

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_URBAN namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_print, jules_message
USE jules_surface_mod, ONLY: l_urban2t, l_aggregate, l_anthrop_heat_src
USE jules_radiation_mod, ONLY: l_cosz
USE jules_surface_types_mod, ONLY: urban_canyon, urban_roof

IMPLICIT NONE

INTEGER :: errcode   ! error code to pass to ereport.

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_URBAN'

IF ( .NOT. l_urban2t ) THEN
  ! If the two-tile urban schemes are not used these switches should be false.
  IF ( ANY ( [ l_urban_empirical,                                              &
                l_moruses_albedo,                                              &
                l_moruses_emissivity,                                          &
                l_moruses_rough,                                               &
                l_moruses_storage,                                             &
                l_moruses_storage_thin,                                        &
                l_moruses_macdonald] ) ) THEN
    errcode = 10
    CALL ereport( RoutineName, errcode,                                        &
                 "l_urban2t=F. All of the urban switches should be .false. .")
  END IF
END IF

! If any of the MORUSES parametrisations are used turn on the umbrella switch,
! l_moruses, which copies the roughness length for momentum (ztm) to z0 in
! sparm.
IF ( ANY ( [ l_moruses_albedo,                                                 &
              l_moruses_emissivity,                                            &
              l_moruses_rough,                                                 &
              l_moruses_storage,                                               &
              l_moruses_macdonald] ) ) THEN
  l_moruses = .TRUE.
  CALL jules_print(RoutineName,                                                &
                   "MORUSES: At least one parametrisation is being used.")
END IF

! Check MORUSES switch logic
IF ( l_urban_empirical .AND. .NOT. l_moruses_macdonald ) THEN
  errcode = 20
  CALL ereport(RoutineName, errcode,                                           &
               "l_urban_empirical=T: Empirical relationships are " //          &
               "being used to provide the urban morphology data. " //          &
               "MacDonald (1998) formulation for roughness length for " //     &
               "momentum and displacement height (l_moruses_macdonald) " //    &
               "must be also be used for consistency.")
END IF

IF ( l_moruses_albedo .AND. .NOT. l_cosz ) THEN
  errcode = 30
  CALL ereport(RoutineName, errcode,                                           &
               "l_moruses_albedo=T: Must also use l_cosz.")
END IF

IF ( l_moruses_storage_thin .AND. .NOT. l_moruses_storage ) THEN
  errcode = 40
  CALL ereport(RoutineName, errcode,                                           &
               "l_moruses_storage_thin=T: MORUSES storage parametrisation " // &
               "should also be used. Currently l_moruses_storage=F.")
END IF

IF ( l_urban2t .AND. l_anthrop_heat_src ) THEN
  IF ( anthrop_heat_scale < 0.0 .OR. anthrop_heat_scale > 1.0 ) THEN
    errcode = 50
    WRITE(jules_message,*)                                                     &
       "anthrop_heat_scale is out of range: anthrop_heat_scale = ",            &
       anthrop_heat_scale
    CALL ereport(RoutineName, errcode, jules_message)
  END IF
END IF

! Set internal switch to perform MORUSES roughness calculations
IF ( l_moruses_rough ) THEN
  IF ( l_aggregate ) THEN
    errcode = 60
    CALL ereport(RoutineName, errcode,                                         &
       "l_aggregate=T and l_moruses_rough=T: " //                              &
       "l_moruses_rough should be .false. .")
  ELSE
    l_moruses_rough_surft(urban_canyon) = .TRUE.
    l_moruses_rough_surft(urban_roof)   = .TRUE.
  END IF
END IF

! Set internal switch to perform MORUSES albedo calculations
! MORUSES only calculates the canyon albedo; roof albedo is set by namelist
IF ( l_moruses_albedo ) l_moruses_albedo_surft(urban_canyon) = .TRUE.

END SUBROUTINE check_jules_urban
#endif

SUBROUTINE print_nlist_jules_urban()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_urban', 'Contents of namelist jules_urban')

WRITE(lineBuffer,*)' anthrop_heat_scale = ',anthrop_heat_scale
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_albedo = ',l_moruses_albedo
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_emissivity = ',l_moruses_emissivity
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_rough = ',l_moruses_rough
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_storage = ',l_moruses_storage
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_storage_thin = ',l_moruses_storage_thin
CALL jules_print('jules_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_macdonald = ',l_moruses_macdonald
CALL jules_print('jules_urban',lineBuffer)

CALL jules_print('jules_urban',                                                &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_urban

#if defined(UM_JULES) && !defined(LFRIC)

SUBROUTINE read_nml_jules_urban (unitnumber)

! Description:
!  Read the JULES_URBAN namelist

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

CHARACTER(LEN=*),   PARAMETER :: RoutineName='READ_NML_JULES_URBAN'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_real = 1
INTEGER, PARAMETER :: n_log = 6

TYPE :: my_namelist
  SEQUENCE
  REAL    :: anthrop_heat_scale
  LOGICAL :: l_moruses_albedo
  LOGICAL :: l_moruses_emissivity
  LOGICAL :: l_moruses_rough
  LOGICAL :: l_moruses_storage
  LOGICAL :: l_moruses_storage_thin
  LOGICAL :: l_moruses_macdonald
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_real_in = n_real,             &
                    n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_urban, IOSTAT = errorstatus,            &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_urban", iomessage)

  my_nml % anthrop_heat_scale     = anthrop_heat_scale
  my_nml % l_moruses_albedo       = l_moruses_albedo
  my_nml % l_moruses_emissivity   = l_moruses_emissivity
  my_nml % l_moruses_rough        = l_moruses_rough
  my_nml % l_moruses_storage      = l_moruses_storage
  my_nml % l_moruses_storage_thin = l_moruses_storage_thin
  my_nml % l_moruses_macdonald    = l_moruses_macdonald
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  anthrop_heat_scale     = my_nml % anthrop_heat_scale
  l_moruses_albedo       = my_nml % l_moruses_albedo
  l_moruses_emissivity   = my_nml % l_moruses_emissivity
  l_moruses_rough        = my_nml % l_moruses_rough
  l_moruses_storage      = my_nml % l_moruses_storage
  l_moruses_storage_thin = my_nml % l_moruses_storage_thin
  l_moruses_macdonald    = my_nml % l_moruses_macdonald

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_urban
#endif

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_urban(nml_dir)

! Description:
!  Read the JULES_URBAN namelist (standalone)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

! Open the urban namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'urban.nml'),                &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_urban",                                                 &
                 "Error opening namelist file urban.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! There are two namelists to read from this file
CALL log_info("init_urban", "Reading JULES_URBAN namelist...")
READ(namelist_unit, NML = jules_urban, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_urban",                                                 &
                 "Error reading namelist JULES_URBAN " //                      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_urban",                                                 &
                 "Error closing namelist file urban.nml " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_urban
#endif

END MODULE jules_urban_mod
