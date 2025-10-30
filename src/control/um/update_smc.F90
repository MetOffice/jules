! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine UPDATE_SMC
!
!  Programming standard : UM documentation paper 3
!
!  Purpose: To update the partitioning between unfrozen and
!           frozen soil moisture after the total soil moisture in a
!           layer has been updated.
!
!           Code Owner: Please refer to ModuleLeaders.txt

MODULE update_smc_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='UPDATE_SMC_MOD'

CONTAINS

SUBROUTINE update_smc (dz_soil)

!Use in relevant subroutines
USE freeze_soil_mod, ONLY: freeze_soil

!USE in JULES modules
USE jules_soil_mod, ONLY: sm_levels

!USE in UM modules (check!)
USE atm_fields_mod, ONLY: clapp_horn, sat_soilw_suction, smcl,                 &
                          deep_soil_temp, vol_smc_sat, sthu, sthf
USE nlsizes_namelist_mod, ONLY: land_field

USE umPrintMgr, ONLY: umPrint, umMessage
USE yomhook,    ONLY: lhook, dr_hook
USE parkind1,   ONLY: jprb, jpim

IMPLICIT NONE

!   Arguments
REAL, INTENT(IN) ::  dz_soil(sm_levels) !IN soil level thicknesses

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='UPDATE_SMC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
WRITE(umMessage,*)                                                             &
 'Partitioning soil moisture in unfrozen and frozen fractions'
CALL umPrint(umMessage,src='update_smc')

CALL freeze_soil(land_field,sm_levels,                                         &
                 clapp_horn, dz_soil, sat_soilw_suction, smcl,                 &
                 deep_soil_temp, vol_smc_sat, sthu, sthf)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE update_smc
END MODULE update_smc_mod
