! ******************************** COPYRIGHT ***********************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! ******************************** COPYRIGHT ***********************************

MODULE deposition_ukca_var_mod

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook
USE ereport_mod, ONLY: ereport

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = "DEPOSITION_UKCA_VAR_MOD"

INTEGER                        :: jpspec_jules  =  1
                                  ! JULES-based variable
                                  ! No. of chemical species in the UKCA
                                  ! chemical mechanism

INTEGER                        :: ndepd_jules   =  1
                                  ! JULES-based variable
                                  ! No. of dry deposited species
                                  ! In the UKCA, ndped = jpdd.
                                  ! These are used interchangeably

INTEGER, ALLOCATABLE           :: nldepd_jules(:)
                                  ! JULES-based variable
                                  ! Holds array elements of the UKCA-based speci,
                                  ! identifying those chemical species in
                                  ! the UKCA chemical mechanism that are
                                  ! deposited

CHARACTER(LEN=10), ALLOCATABLE :: speci_jules(:)
                                  ! JULES-based variable
                                  ! Names of all the chemical species
                                  ! in the UKCA chemical mechanism

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE deposition_ukca_var_alloc(jpspec)
!-----------------------------------------------------------------------------

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN)           :: jpspec

!Local variables
INTEGER :: i
INTEGER :: ERROR, error_sum  ! Error indicators

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DEPOSITION_UKCA_VAR_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ERROR = 0

! Allocate arrays
ALLOCATE( nldepd_jules(jpspec), STAT = ERROR )
error_sum = ERROR
ALLOCATE( speci_jules(jpspec), STAT = ERROR )
error_sum = error_sum + ERROR

nldepd_jules(:) = 0
speci_jules(:) = '          '

IF ( error_sum /= 0 ) THEN
  CALL ereport (TRIM(RoutineName), error_sum,                                  &
                "Please check allocation of arrays")
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_ukca_var_alloc

!-----------------------------------------------------------------------------
#if defined(UM_JULES)
! Only for JULES in the UM
! Check that the JULES-based and UKCA deposition switches are the same
SUBROUTINE deposition_check_ukca_switches()
!-----------------------------------------------------------------------------

USE jules_deposition_mod, ONLY:                                                &
    l_ukca_ddep_lev1_jules => l_ukca_ddep_lev1,                                &
    l_ukca_ddepo3_ocean_jules => l_ukca_ddepo3_ocean,                          &
    l_ukca_dry_dep_so2wet_jules => l_ukca_dry_dep_so2wet,                      &
    l_ukca_emsdrvn_ch4_jules => l_ukca_emsdrvn_ch4

USE jules_science_fixes_mod, ONLY:                                             &
    l_fix_drydep_so2_water_jules => l_fix_drydep_so2_water,                    &
    l_fix_improve_drydep_jules => l_fix_improve_drydep,                        &
    l_fix_ukca_h2dd_x_jules => l_fix_ukca_h2dd_x

USE ukca_option_mod, ONLY:                                                     &
    l_ukca_ddep_lev1_ukca => l_ukca_ddep_lev1,                                 &
    l_ukca_ddepo3_ocean_ukca => l_ukca_ddepo3_ocean,                           &
    l_ukca_dry_dep_so2wet_ukca => l_ukca_dry_dep_so2wet,                       &
    l_ukca_emsdrvn_ch4_ukca => l_ukca_emsdrvn_ch4

USE science_fixes_mod, ONLY:                                                   &
    l_fix_drydep_so2_water_ukca => l_fix_drydep_so2_water,                     &
    l_fix_improve_drydep_ukca => l_fix_improve_drydep,                         &
    l_fix_ukca_h2dd_x_ukca => l_fix_ukca_h2dd_x

USE umPrintMgr, ONLY: newline, umMessage

IMPLICIT NONE

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

INTEGER                       :: error_code         ! Error code
CHARACTER(LEN=*), PARAMETER   :: RoutineName='DEPOSITION_CHECK_UKCA_SWITCHES'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Check l_ukca_ddep_lev1
error_code = -1
IF (l_ukca_ddep_lev1_jules .NEQV. l_ukca_ddep_lev1_ukca) THEN
  l_ukca_ddep_lev1_jules = l_ukca_ddep_lev1_ukca
  umMessage='WARNING: Mismatch in l_ukca_ddep_lev1' //                newline//&
    'for JULES with atmospheric deposition between run_ukca and '            //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from run_ukca namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

! Check l_ukca_ddepo3_ocean
error_code = -1
IF (l_ukca_ddepo3_ocean_jules .NEQV. l_ukca_ddepo3_ocean_ukca) THEN
  l_ukca_ddepo3_ocean_jules = l_ukca_ddepo3_ocean_ukca
  umMessage='WARNING: Mismatch in l_ukca_ddepo3_ocean' //             newline//&
    'for JULES with atmospheric deposition between run_ukca and '            //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from run_ukca namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

! Check l_ukca_dry_dep_so2wet
error_code = -1
IF (l_ukca_dry_dep_so2wet_jules .NEQV. l_ukca_dry_dep_so2wet_ukca) THEN
  l_ukca_dry_dep_so2wet_jules = l_ukca_dry_dep_so2wet_ukca
  umMessage='WARNING: Mismatch in l_ukca_dry_dep_so2wet ' //          newline//&
    'for JULES with atmospheric deposition between run_ukca and '            //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from run_ukca namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

! Check l_ukca_emsdrvn_ch4
error_code = -1
IF (l_ukca_emsdrvn_ch4_jules .NEQV. l_ukca_emsdrvn_ch4_ukca) THEN
  l_ukca_emsdrvn_ch4_jules = l_ukca_emsdrvn_ch4_ukca
  umMessage='WARNING: Mismatch in l_ukca_emsdrvn_ch4' //              newline//&
    'for JULES with atmospheric deposition between run_ukca and '            //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from run_ukca namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

! Check l_fix_drydep_so2_water
error_code = -1
IF (l_fix_drydep_so2_water_jules .NEQV. l_fix_drydep_so2_water_ukca) THEN
  l_fix_drydep_so2_water_jules = l_fix_drydep_so2_water_ukca
  umMessage='WARNING: Mismatch in l_fix_drydep_so2_water' //          newline//&
    'for JULES with atmospheric deposition between temp_fixes and '          //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from temp_fixes namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

! Check l_fix_improve_drydep
error_code = -1
IF (l_fix_improve_drydep_jules .NEQV. l_fix_improve_drydep_ukca) THEN
  l_fix_improve_drydep_jules = l_fix_improve_drydep_ukca
  umMessage='WARNING: Mismatch in l_fix_improve_drydep' //            newline//&
    'for JULES with atmospheric deposition between temp_fixes and '          //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from temp_fixes namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

! Check l_fix_ukca_h2dd_x
error_code = -1
IF (l_fix_ukca_h2dd_x_jules .NEQV. l_fix_ukca_h2dd_x_ukca) THEN
  l_fix_ukca_h2dd_x_jules = l_fix_ukca_h2dd_x_ukca
  umMessage='WARNING: Mismatch in l_fix_ukca_h2dd_x' //               newline//&
    'for JULES with atmospheric deposition between temp_fixes and '          //&
    'jules_deposition namelists' //                                   newline//&
    'Using setting from temp_fixes namelist'
  CALL ereport(RoutineName, error_code, umMessage)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_check_ukca_switches
#endif

END MODULE deposition_ukca_var_mod
