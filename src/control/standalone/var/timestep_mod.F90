#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!  *** JULES version of timestep_mod ***

MODULE timestep_mod

! Description:
!   Holds the length of the model timestep as a real number for use
!   in science routines and consistency with the UM. It is set in
!   init_time from the variable timestep_len.

! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL

! Code Description:
!   Language: FORTRAN 95.
!   This code is written to JULES coding standards v1.

IMPLICIT NONE

REAL    :: timestep             ! atmosphere model timestep (s)

END MODULE timestep_mod
#endif
