#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_inferno_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_inferno(nml_dir)

USE jules_inferno_mod, ONLY: read_nml_jules_inferno,                           &
                           print_nlist_jules_inferno,                          &
                           check_jules_inferno,                                &
                           l_inferno, l_trif_fire

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the inferno fire model parameters and properties
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_inferno(nml_dir)

CALL print_nlist_jules_inferno()

IF ( .NOT. l_inferno .AND. .NOT. l_trif_fire ) RETURN

CALL check_jules_inferno()

RETURN

END SUBROUTINE init_inferno
END MODULE init_inferno_mod
#endif
