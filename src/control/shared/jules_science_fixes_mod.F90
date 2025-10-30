! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Description:
!   This module declares 'short-term' temporary logicals used to protect
!   science bug fixes that lead to significant alterations in science results.
!   It is expected that these logicals will be short lived as the preference
!   should be for all configurations to use the corrected code. But
!   to maintain short term reproducibility of results across JULES versions
!   the fixes are protected by logicals until the fixes become the default
!   in all model configurations and the logical is retired.

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3

MODULE jules_science_fixes_mod

USE errormessagelength_mod, ONLY: errormessagelength
USE jules_sea_seaice_mod,   ONLY: l_sice_multilayers
USE missing_data_mod, ONLY: imdi

IMPLICIT NONE

! Set this logical to .TRUE. to correct the updating of the surface
! temperature in the implicit solver.
! ticket #106 (um:#575)
LOGICAL :: l_dtcanfix = .FALSE.               ! Review Again May 2016

! Fixes the calculation of surface exchange in coastal grid-boxes when
! coastal tiling is switched on. Should have no effect in standalone
! JULES.
! ticket #177,#1184 (um:#1017)
INTEGER :: ctile_orog_fix = imdi
INTEGER, PARAMETER :: correct_sea_adjust_land = 1
INTEGER, PARAMETER :: correct_sea_only = 2

! Fixes how ustar is included in the exchange coefficient for dust deposition.
! Has no effect in standalone JULES.
! ticket #251 (um:#1729)
LOGICAL :: l_fix_ustar_dust = .FALSE.     ! Review again Jan 2018

! Fixes bug in ice thickness used in sea ice albedo calculation when
! multilayer sea ice is used.
! Has no effect in standalone JULES.
! ticket #547 (um:#3080)
LOGICAL :: l_fix_alb_ice_thick = .FALSE.  ! Review in Nov 2018

! Enables ice layer temperatures to correctly evolve over time at
! sea points not coupled to the ocean. These are typically lake points.
LOGICAL :: l_fix_lake_ice_temperatures = .FALSE.

! Corrects the calculation of the albedo of snow in the two-stream
! scheme.
! ticket #533 (um:#3011)
LOGICAL :: l_fix_albsnow_ts = .FALSE.     ! Review in Dec 2020

! Fixes a bug that means the unloading of snow from vegetation would
! potentially and incorrectly use a wind speed of zero.
! Has no effect in standalone JULES.
! ticket #740 (um:#4038)
LOGICAL :: l_fix_wind_snow = .FALSE.      ! Review in May 2019

! ticket #610 (um:#4332)
LOGICAL :: l_fix_moruses_roof_rad_coupling = .FALSE. ! Review in May 2019

! ticket #874 (um:#4581)
LOGICAL :: l_fix_osa_chloro = .FALSE.     ! Review in Jan 2020

! ticket # (Jules:#194)
LOGICAL :: l_accurate_rho = .FALSE.       ! Review in Jan 2022

! ticket # (Jules:#1279)
LOGICAL :: l_fix_snow_frac = .FALSE.       ! Review in April 2024

! ticket # (Jules:#1396)
LOGICAL :: l_fix_neg_snow = .FALSE.       ! Review in April 2025

! ticket #931 (um:#6885): Atmospheric deposition fixes used in UKCA
LOGICAL :: l_fix_drydep_so2_water = .FALSE. ,                                  &
             ! True to use correct surface resistance of water when calculating
             ! dry deposition of SO2
           l_fix_improve_drydep = .FALSE. ,                                    &
             ! True to fix dry deposition velocities
           l_fix_ukca_h2dd_x = .FALSE.
             ! True to fix H2 deposition to shrub/soil


NAMELIST  /jules_temp_fixes/                                                   &
         l_dtcanfix, ctile_orog_fix, l_fix_ustar_dust, l_fix_alb_ice_thick,    &
         l_fix_albsnow_ts, l_fix_wind_snow, l_fix_moruses_roof_rad_coupling,   &
         l_fix_osa_chloro, l_accurate_rho, l_fix_lake_ice_temperatures,        &
         l_fix_snow_frac, l_fix_drydep_so2_water, l_fix_improve_drydep,        &
         l_fix_ukca_h2dd_x, l_fix_neg_snow

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SCIENCE_FIXES_MOD'

CONTAINS

SUBROUTINE print_nlist_jules_temp_fixes()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print(ModuleName, 'Contents of namelist jules_temp_fixes')

WRITE(lineBuffer,'(A,L1)') ' l_dtcanfix = ',          l_dtcanfix
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,I0)') ' ctile_orog_fix = ',    ctile_orog_fix
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_ustar_dust = ',    l_fix_ustar_dust
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_alb_ice_thick = ', l_fix_alb_ice_thick
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_lake_ice_temperatures = ', l_fix_lake_ice_temperatures
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_albsnow_ts = ',    l_fix_albsnow_ts
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_wind_snow = ',     l_fix_wind_snow
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_moruses_roof_rad_coupling = ',              &
                             l_fix_moruses_roof_rad_coupling
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_osa_chloro = ',    l_fix_osa_chloro
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_accurate_rho = ',      l_accurate_rho
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_snow_frac = ',     l_fix_snow_frac
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_neg_snow = ',      l_fix_neg_snow
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_improve_drydep = ', l_fix_improve_drydep
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_ukca_h2dd_x = ',   l_fix_ukca_h2dd_x
CALL jules_print(ModuleName,lineBuffer)
WRITE(lineBuffer,'(A,L1)') ' l_fix_drydep_so2_water = ', l_fix_drydep_so2_water
CALL jules_print(ModuleName,lineBuffer)

CALL jules_print(ModuleName,                                                   &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_temp_fixes

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_temp_fixes(nml_dir)

! Description:
!  Read the JULES_TEMP_FIXES namelist (standalone)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

! Open the urban namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'science_fixes.nml'),        &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(ModuleName,                                                   &
                 "Error opening namelist file science_fixes.nml " //           &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Read the jules_temp_fixes namelist
CALL log_info(ModuleName, "Reading JULES_TEMP_FIXES namelist...")
READ(namelist_unit, NML = jules_temp_fixes, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(ModuleName,                                                   &
                 "Error reading namelist JULES_TEMP_FIXES " //                 &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(ModuleName,                                                   &
                 "Error closing namelist file science_fixes.nml " //           &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_temp_fixes

SUBROUTINE init_science_fixes(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the jules_temp_fixes namelist items
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_temp_fixes(nml_dir)
CALL print_nlist_jules_temp_fixes()
CALL warn_jules_temp_fixes()

RETURN

END SUBROUTINE init_science_fixes
#endif

#if defined(UM_JULES) && !defined(LFRIC)

SUBROUTINE read_nml_jules_temp_fixes (unit_in)

! Description:
!  Read the JULES_TEMP_FIXES namelist (UM)

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unit_in

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_TEMP_FIXES'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_int = 1
INTEGER, PARAMETER :: n_log = 14

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: ctile_orog_fix
  LOGICAL :: l_dtcanfix
  LOGICAL :: l_fix_ustar_dust
  LOGICAL :: l_fix_alb_ice_thick
  LOGICAL :: l_fix_lake_ice_temperatures
  LOGICAL :: l_fix_albsnow_ts
  LOGICAL :: l_fix_wind_snow
  LOGICAL :: l_fix_moruses_roof_rad_coupling
  LOGICAL :: l_fix_osa_chloro
  LOGICAL :: l_accurate_rho
  LOGICAL :: l_fix_snow_frac
  LOGICAL :: l_fix_neg_snow
  LOGICAL :: l_fix_drydep_so2_water
  LOGICAL :: l_fix_improve_drydep
  LOGICAL :: l_fix_ukca_h2dd_x
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int, n_log_in = n_log)

IF (mype == 0) THEN

  READ(UNIT = unit_in, NML = jules_temp_fixes, IOSTAT = ErrorStatus,           &
     IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_temp_fixes", iomessage)

  my_nml % ctile_orog_fix                  = ctile_orog_fix
  my_nml % l_dtcanfix                      = l_dtcanfix
  my_nml % l_fix_ustar_dust                = l_fix_ustar_dust
  my_nml % l_fix_alb_ice_thick             = l_fix_alb_ice_thick
  my_nml % l_fix_lake_ice_temperatures     = l_fix_lake_ice_temperatures
  my_nml % l_fix_albsnow_ts                = l_fix_albsnow_ts
  my_nml % l_fix_wind_snow                 = l_fix_wind_snow
  my_nml % l_fix_moruses_roof_rad_coupling = l_fix_moruses_roof_rad_coupling
  my_nml % l_fix_osa_chloro                = l_fix_osa_chloro
  my_nml % l_accurate_rho                  = l_accurate_rho
  my_nml % l_fix_snow_frac                 = l_fix_snow_frac
  my_nml % l_fix_neg_snow                  = l_fix_neg_snow
  my_nml % l_fix_drydep_so2_water          = l_fix_drydep_so2_water
  my_nml % l_fix_improve_drydep            = l_fix_improve_drydep
  my_nml % l_fix_ukca_h2dd_x               = l_fix_ukca_h2dd_x
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN
  ctile_orog_fix                  = my_nml % ctile_orog_fix
  l_dtcanfix                      = my_nml % l_dtcanfix
  l_fix_ustar_dust                = my_nml % l_fix_ustar_dust
  l_fix_alb_ice_thick             = my_nml % l_fix_alb_ice_thick
  l_fix_lake_ice_temperatures     = my_nml % l_fix_lake_ice_temperatures
  l_fix_albsnow_ts                = my_nml % l_fix_albsnow_ts
  l_fix_wind_snow                 = my_nml % l_fix_wind_snow
  l_fix_moruses_roof_rad_coupling = my_nml % l_fix_moruses_roof_rad_coupling
  l_fix_osa_chloro                = my_nml % l_fix_osa_chloro
  l_accurate_rho                  = my_nml % l_accurate_rho
  l_fix_snow_frac                 = my_nml % l_fix_snow_frac
  l_fix_neg_snow                  = my_nml % l_fix_neg_snow
  l_fix_drydep_so2_water          = my_nml % l_fix_drydep_so2_water
  l_fix_improve_drydep            = my_nml % l_fix_improve_drydep
  l_fix_ukca_h2dd_x               = my_nml % l_fix_ukca_h2dd_x
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_temp_fixes
#endif

SUBROUTINE warn_jules_temp_fixes()

USE ereport_mod, ONLY: ereport
USE jules_surface_mod, ONLY: off
USE jules_print_mgr, ONLY: newline

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: RoutineName='WARN_JULES_TEMP_FIXES'

INTEGER :: errorstatus
CHARACTER(LEN=errormessagelength) :: cmessage

IF ( .NOT. l_dtcanfix ) THEN
  ErrorStatus = -100
  CMessage    = 'Model run excludes ticket um:#575 as'//                       &
                ' l_dtcanfix=.FALSE.'                 //                       &
                ' This will affect any model run.'
  CALL ereport(RoutineName, ErrorStatus, CMessage)
END IF

IF ( .NOT. l_fix_alb_ice_thick ) THEN
  ErrorStatus = -100
  cmessage    =                                                      newline// &
  'Model run excludes a change from JULES ticket 547 as'//           newline// &
  ' l_fix_alb_ice_thick=.FALSE.'//                                   newline// &
  'This will affect any model runs where l_sice_multilayers is    '//newline// &
  '.TRUE. and will result in an incorrect sea ice thickness being '//newline// &
  'used in the calculation of bare ice albedo.'
  CALL ereport(RoutineName, ErrorStatus, CMessage)
END IF

IF ( .NOT. l_fix_albsnow_ts ) THEN
  ErrorStatus = -100
  cmessage    =                                                      newline// &
  'Model run excludes a change from um:#3011 as'//                   newline// &
  ' l_fix_albsnow_ts=.FALSE.'//                                      newline// &
  'This affects the albedo of snow as calculated in the two-stream'//newline// &
  'scheme in JULES.'
  CALL ereport(RoutineName, ErrorStatus, CMessage)
END IF

IF ( ctile_orog_fix <= off ) THEN
  ErrorStatus = -100
  CMessage    = 'Model run excludes um:#1017 as'    // newline//               &
                ' fix_ctile_orog=off'               // newline//               &
                ' This will affect runs with coastal tiling.'
  CALL ereport(RoutineName, ErrorStatus, CMessage)
ELSE IF ( ctile_orog_fix == correct_sea_adjust_land ) THEN
  ErrorStatus = -100
  CMessage    = 'Model run excludes um:#1184 as'          // newline//         &
                ' fix_ctile_orog=correct_sea_adjust_land' // newline//         &
                ' This will affect runs with coastal tiling.'
  CALL ereport(RoutineName, ErrorStatus, CMessage)
END IF

IF ( .NOT. l_fix_moruses_roof_rad_coupling ) THEN
  errorstatus = -100
  cmessage    =                                                    newline //  &
  'jules:#610 fix to the radiative roof coupling is not enabled: ' //          &
  'l_fix_moruses_roof_rad_coupling = .FALSE.'
  CALL ereport(RoutineName, errorstatus, cmessage)
END IF

IF ( .NOT. l_fix_ustar_dust ) THEN
  ErrorStatus = -100
  cmessage    =                                                      newline// &
  'Model run excludes um:#1729 as l_fix_ustar_dust=.FALSE.'//        newline// &
  'This will affect any model runs which include interactive dust.'
  CALL ereport(RoutineName, ErrorStatus, CMessage)
END IF

IF ( .NOT. l_fix_wind_snow ) THEN
  ErrorStatus = -100
  cmessage    =                                                      newline// &
  'Model run excludes ticket um:#4038 as l_fix_wind_snow=.FALSE.. '//newline// &
  'This will mean that a zero wind speed will incorrectly be used '//newline// &
  'in the calculation of wind-dependent unloading of snow from '//   newline// &
  'vegetation on timesteps when 10m wind diagnostics are not requested.'
  CALL ereport(RoutineName, ErrorStatus, cmessage)
END IF

IF ( .NOT. l_fix_osa_chloro) THEN
  ErrorStatus = -100
  cmessage    =                                                       newline//&
  'Model run excludes a change from ticket um:#4581 as'//             newline//&
  ' l_fix_osa_chloro=.FALSE.'//                                       newline//&
  ' This will mean that chlorophyll used for the ocean albedo is' // newline// &
  ' used in gm-3 when it should be mg m-3'
  CALL ereport(RoutineName, ErrorStatus, cmessage)
END IF

IF ( .NOT. l_accurate_rho) THEN
  ErrorStatus = -100
  cmessage    =                                                       newline//&
  'Model run excludes a change from ticket jules:#194 as'//           newline//&
  ' l_accurate_rho=.FALSE.'//                                         newline//&
  ' This will mean that an inaccurate estimate of surface air '    // newline//&
  ' density will be used'
  CALL ereport(RoutineName, ErrorStatus, cmessage)
END IF

IF ( .NOT. l_fix_snow_frac ) THEN
  errorstatus = -100
  cmessage    =                                                     newline // &
  'jules:#1279 fix to remove persistent small snow amounts' //      newline // &
  ' when using the frac_snow_subl_melt=1 option is not enabled: ' //newline // &
  'l_fix_snow_frac = .FALSE.'
  CALL ereport(RoutineName, errorstatus, cmessage)
END IF

IF ( .NOT. l_fix_neg_snow ) THEN
  errorstatus = -100
  cmessage    =                                                     newline // &
  'jules:#1396 fix to remove correct the melting calculation'     //newline // &
  ' that can result in negative snow amounts is not enabled: '    //newline // &
  'l_fix_neg_snow = .FALSE.'
  CALL ereport(RoutineName, errorstatus, cmessage)
END IF

IF ( ( .NOT. l_fix_lake_ice_temperatures ) .AND. l_sice_multilayers) THEN
  ErrorStatus = -100
  cmessage    =                                                      newline// &
  'Model run excludes a change from JULES ticket 1161 as'//          newline// &
  ' l_fix_lake_ice_temperatures=.FALSE.'//                           newline// &
  'This will affect coupled NWP models that have lakes as sea     '//newline// &
  'points that are uncoupled to the ocean. Ice layer temperatures '//newline// &
  'in these lakes will not evolve correctly over time.  '
  CALL ereport(RoutineName, ErrorStatus, CMessage)
END IF

! JULES and atmospheric deposition
! Copied from um/src/control/misc/science_fixes_mod.F90
IF (.NOT. l_fix_improve_drydep) THEN
  ErrorStatus = -100
  cmessage    =                                                       newline//&
  'Model run excludes a change from ticket um:#1250 as'//             newline//&
  ' l_fix_improve_drydep=.FALSE.'//                                   newline//&
  ' This will mean that dry deposition velocities are set to null'//  newline//&
  ' for HCl, HOCl, HBr, HOBr, H2SO4, MeOH and Sec_Org and that dry'// newline//&
  ' deposition velocities for 9 tiles are inconsistent with 13/17/27 tiles.'
  CALL ereport(RoutineName, ErrorStatus, Cmessage)
END IF

IF (.NOT. l_fix_drydep_so2_water) THEN
  ErrorStatus = -100
  cmessage    =                                                       newline//&
  ' Model run excludes a change from ticket um:#5651, '//             newline//&
  ' l_fix_drydep_so2_water=.FALSE.'//                                 newline//&
  ' This will mean that the surface resistance of water when '//      newline//&
  ' calculating the dry deposition of SO2 is too high for 13, 17 '//  newline//&
  ' and 27 tiles.'
  CALL ereport(RoutineName, ErrorStatus, Cmessage)
END IF

IF (.NOT.  l_fix_ukca_h2dd_x) THEN
  ErrorStatus = -100
  cmessage    =                                                       newline//&
  'Model run excludes a change from ticket um:#4157 as'//             newline//&
  ' l_fix_ukca_h2dd_x=.FALSE. .'//                                    newline//&
  ' This will mean that the wrong element is used for h2dd_c & h2dd_m.'
  CALL ereport(RoutineName, ErrorStatus, Cmessage)
END IF

END SUBROUTINE warn_jules_temp_fixes

END MODULE jules_science_fixes_mod
