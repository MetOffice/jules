#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_rivers_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_rivers(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the river routing and overbank inundation namelist items, and
!   checks them for consistency.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: read_nml_jules_rivers, print_nlist_jules_rivers,   &
                            check_jules_rivers, l_riv_overbank

USE overbank_inundation_mod, ONLY: read_nml_jules_overbank,                    &
                                   print_nlist_jules_overbank,                 &
                                   check_jules_overbank

IMPLICIT NONE


! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_rivers(nml_dir)
CALL print_nlist_jules_rivers()
CALL check_jules_rivers()
IF ( l_riv_overbank ) THEN
  CALL read_nml_jules_overbank(nml_dir)
  CALL print_nlist_jules_overbank()
  CALL check_jules_overbank()
END IF

END SUBROUTINE init_rivers

END MODULE init_rivers_mod
#endif
