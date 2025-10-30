#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_model_environment_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_model_environment(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the surface namelist items and checks them for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_model_environment_mod, ONLY:                                         &
    read_nml_jules_model_environment,                                          &
    print_nlist_jules_model_environment,                                       &
    check_jules_model_environment

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_model_environment(nml_dir)
CALL print_nlist_jules_model_environment()
CALL check_jules_model_environment()

RETURN

END SUBROUTINE init_model_environment

END MODULE init_model_environment_mod
#endif
