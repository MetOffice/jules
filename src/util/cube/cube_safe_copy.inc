#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE cube_safe_copy(c1, c2)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Copies cube c2 to cube c1 in a memory safe way
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN OUT) :: c1  ! The cube to copy to
TYPE(data_cube), INTENT(IN) :: c2     ! The cube to copy


!-----------------------------------------------------------------------------


! First, make sure the cube we are copying to is deallocated
CALL cube_free(c1)

! Create a new cube of the correct shape
c1 = cube_create(c2%SHAPE)

! Copy the values across
c1%values(:) = c2%values(:)

RETURN

END SUBROUTINE cube_safe_copy
#endif
