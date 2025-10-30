#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_initial_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_initial(nml_dir)

USE jules_initial_mod, ONLY: read_nml_jules_initial,                           &
                             check_jules_initial

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads and checks jules_initial namelist
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

CALL read_nml_jules_initial(nml_dir)
CALL check_jules_initial()

RETURN

END SUBROUTINE init_initial
END MODULE init_initial_mod
#endif
