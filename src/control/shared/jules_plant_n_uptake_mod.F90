#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE jules_plant_n_uptake_mod


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Contains plant N uptake variables. In time there will be a namelist for
!   setting options - but not in this version.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Public scope by default

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
! Parameters identifying alternative plant N uptake models.
! These should be >0 and unique.
! At present there is only one possibility.
INTEGER, PARAMETER ::                                                          &
  n_model_triffid = 1
     ! Plant uptake of N is calculated by TRIFFID.

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Integer variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  n_uptake_model = 0
    ! Switch for model of uptake of N by plants.
    ! 0 = no model. Other values are given by n_model_* parameters.

CONTAINS

!#############################################################################

#if !defined(RIVERS_ONLY)
SUBROUTINE check_jules_plant_n_uptake()

USE jules_surface_mod, ONLY:                                                   &
!  imported scalars (IN)
     l_aggregate

USE jules_vegetation_mod, ONLY:                                                &
!  imported scalars (IN)
       l_triffid

USE ereport_mod, ONLY:                                                         &
  ereport

USE string_utils_mod, ONLY: to_string

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_PLANT_N_UPTAKE namelist for consistency.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Local scalar parameters.
INTEGER :: errorstatus
! Value of error flag to indicate a hard or fatal error.

CHARACTER(LEN=*), PARAMETER ::                                                 &
   RoutineName = 'check_jules_plant_n_uptake'   ! Name of this procedure.

!-----------------------------------------------------------------------------
! In future versions the choice of model will be read from a namelist.
! For this version, if TRIFFID is selected indicate that that will deal with
! N uptake.
!-----------------------------------------------------------------------------
IF ( l_triffid ) n_uptake_model = n_model_triffid

!-----------------------------------------------------------------------------
! If no N uptake model is selected, we have nothing more to do.
!-----------------------------------------------------------------------------
IF ( n_uptake_model == 0 ) RETURN

!-----------------------------------------------------------------------------
! Check a valid model is selected. Currently redundant, but needed in future!
!-----------------------------------------------------------------------------
SELECT CASE ( n_uptake_model )
CASE ( n_model_triffid )
  !     This is OK.
CASE DEFAULT
  errorstatus = 101
  CALL ereport( TRIM(routineName), errorstatus,                                &
                "Unknown N uptake model: " // to_string(n_uptake_model) )
END SELECT

!-----------------------------------------------------------------------------
! For now, insist that an N model can only be used with a veg model (TRIFFID).
!-----------------------------------------------------------------------------
IF ( .NOT. l_triffid ) THEN
  errorstatus = 101
  CALL ereport( RoutineName, errorstatus,                                      &
                'A plant N uptake model requires that a veg model ' //         &
                '(TRIFFID) is used.' )
END IF

!-----------------------------------------------------------------------------
! Plant N uptake cannot be used with the aggregate surface scheme.
! At present this follows from needing TRIFFID, but test again anyway.
!-----------------------------------------------------------------------------
IF ( l_aggregate ) THEN
  errorstatus = 101
  CALL ereport( RoutineName, errorstatus,                                      &
                'Plant N uptake cannot be used with the ' //                   &
                'aggregated surface scheme (l_aggregate = true)')
END IF

END SUBROUTINE check_jules_plant_n_uptake
#endif

!#############################################################################

END MODULE jules_plant_n_uptake_mod
#endif
