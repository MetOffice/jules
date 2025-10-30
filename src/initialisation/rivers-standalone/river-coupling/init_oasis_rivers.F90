! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_oasis_rivers_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_oasis_rivers(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the river coupling namelist items, and
!   checks them for consistency.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
USE string_utils_mod, ONLY: to_string

USE oasis_rivers_mod, ONLY: read_nml_oasis_rivers, check_oasis_rivers,         &
                            print_nlist_oasis_rivers

USE logging_mod, ONLY: log_info

IMPLICIT NONE


! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
INTEGER :: ERROR  ! Error indicator

!-----------------------------------------------------------------------------
! Read river routing namelist
!----------------------------------------------------------------------------
CALL log_info("init_oasis_rivers", "Reading OASIS_RIVERS namelist...")

CALL read_nml_oasis_rivers(nml_dir)

! Print namelist values
CALL print_nlist_oasis_rivers()

! Check namelist values.
CALL check_oasis_rivers()

END SUBROUTINE init_oasis_rivers

END MODULE init_oasis_rivers_mod
