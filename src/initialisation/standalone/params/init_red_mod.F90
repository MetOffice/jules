#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_red_mod

!-----------------------------------------------------------------------------
! Description:
!   Module containing routine to read the Robust Ecosystem Demography (red)
!   namelists in standalone model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

IMPLICIT NONE

PRIVATE
PUBLIC init_red

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE init_red(nml_dir,land_pts,nsurft,nnpft,npft,nmasst)
!-----------------------------------------------------------------------------

USE jules_vegetation_mod, ONLY: l_red

USE red_io, ONLY: read_nml_jules_red, print_nlist_jules_red

USE veg3_parm_mod, ONLY: veg3_parm_init, check_jules_red_parms

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN).
!-----------------------------------------------------------------------------
! Arguments
INTEGER, INTENT(IN) :: land_pts, nnpft, npft, nsurft, nmasst

CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!-----------------------------------------------------------------------------
! Nothing to do if red is not selected.
!-----------------------------------------------------------------------------
IF ( .NOT. l_red ) RETURN

!-----------------------------------------------------------------------------
! Read red parameters namelist
!-----------------------------------------------------------------------------
CALL read_nml_jules_red(nml_dir)

CALL print_nlist_jules_red()

CALL veg3_parm_init(land_pts,nsurft,nnpft,npft,nmasst)

CALL check_jules_red_parms()

RETURN

END SUBROUTINE init_red

END MODULE init_red_mod
#endif
