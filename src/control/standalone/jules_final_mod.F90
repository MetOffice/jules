#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE jules_final_mod

IMPLICIT NONE

CONTAINS

!#############################################################################

SUBROUTINE jules_final

USE jules_deposition_mod, ONLY:                                                &
  ! imported scalars
  l_deposition

USE jules_soil_biogeochem_mod, ONLY:                                           &
!  imported scalar parameters
    soil_model_ecosse,                                                         &
!  imported scalars (IN)
    soil_bgc_model

USE jules_water_resources_mod, ONLY:                                           &
  ! imported scalars
  l_water_resources

USE logging_mod, ONLY:                                                         &
!  imported procedures
    log_error, log_warn

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes final warning messages at the end of a JULES run.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local scalar parameters.
CHARACTER(LEN=*), PARAMETER ::                                                 &
  proc_name = 'jules_final'   !  Name of this procedure.

!-----------------------------------------------------------------------------
! Warn if the ECOSSE soil model is used - it is not fully functional in this
! version.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  CALL log_warn(proc_name,'#############################################')
  CALL log_warn(proc_name, 'The ECOSSE soil model is not fully '    //         &
                           'functional in this version. It should ' //         &
                           'not be used!')
  CALL log_warn(proc_name,'#############################################')
END IF

!-----------------------------------------------------------------------------
! Raise an error if deposition is selected - it is not fully functional in
! this version.
!-----------------------------------------------------------------------------
IF ( l_deposition ) THEN
  CALL log_warn(proc_name, 'The deposition code is available in this ' //      &
                           'version but should be used with caution '  //      &
                           'as it is still under development')
END IF

!-----------------------------------------------------------------------------
! Warn if the water resource model is used - it is still under development.
!-----------------------------------------------------------------------------
IF ( l_water_resources ) THEN
  CALL log_warn(proc_name, '#############################################')
  CALL log_warn(proc_name, 'Water resource modelling (l_water_resources) ' //  &
                           'is still under development. It should be '     //  &
                           'used with caution!')
  CALL log_warn(proc_name, '#############################################')
END IF

END SUBROUTINE jules_final

!#############################################################################

END MODULE jules_final_mod
#endif
