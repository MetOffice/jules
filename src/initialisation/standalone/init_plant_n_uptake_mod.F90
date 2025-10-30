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
MODULE init_plant_n_uptake_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_plant_n_uptake(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Sets plant N uptake variables. Later versions will read a namelist, but
!   this version does not.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_plant_n_uptake_mod, ONLY: check_jules_plant_n_uptake

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
     routineName = 'init_plant_n_uptake'   ! Name of this routine.

!-----------------------------------------------------------------------------
! Check values of generic switches.
!-----------------------------------------------------------------------------
CALL check_jules_plant_n_uptake()

RETURN
END SUBROUTINE init_plant_n_uptake

END MODULE init_plant_n_uptake_mod
#endif
