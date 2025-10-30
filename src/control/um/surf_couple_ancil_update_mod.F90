#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE surf_couple_ancil_update_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='SURF_COUPLE_ANCIL_UPDATE_MOD'

CONTAINS

SUBROUTINE surf_couple_ancil_update(smc_updated, dz_soil)

!Use in relevant subroutines
USE update_veg_mod, ONLY: update_veg
USE update_smc_mod, ONLY: update_smc
USE jules_irrig_mod, ONLY: l_irrig_dmd
USE assign_irrig_fraction_mod,  ONLY: assign_irrig_fraction
USE update_irrig_variables_mod, ONLY: update_irrig_variables

USE jules_soil_mod, ONLY: sm_levels

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

!USE in instances of the JULES TYPES
USE atm_fields_mod, ONLY: psparms,crop_vars,ainfo

IMPLICIT NONE

!Argument
LOGICAL,  INTENT(IN) :: smc_updated
REAL,     INTENT(IN) :: dz_soil(sm_levels) !IN soil level thicknesses

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SURF_COUPLE_ANCIL_UPDATE'

!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Update vegetation parameters
CALL update_veg()

! Update partitioning of unfrozen and frozen soil moisture
! if soil moisture updated
IF (smc_updated) THEN
  CALL update_smc(dz_soil)
END IF

! Update irrigated fraction and soil moisture in the irrigated fraction
IF ( l_irrig_dmd ) THEN
  CALL assign_irrig_fraction(crop_vars,ainfo)
  CALL update_irrig_variables(crop_vars,psparms,ainfo)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_ancil_update
END MODULE surf_couple_ancil_update_mod
#endif
