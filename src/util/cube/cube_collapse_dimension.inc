#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE cube_collapse_dimension(c1, c2, DIM)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Copies data from a 2d cube with a degenerate dimension into a 1d cube,
!   in a memory safe way.
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
TYPE(data_cube), INTENT(IN) :: c2     ! The cube to copy from

INTEGER, INTENT(IN) :: DIM ! The degenerate dimension to collapse


!-----------------------------------------------------------------------------


! Check a valid dimension has been requested
IF ( ( DIM < 1 ) .OR. ( DIM > SIZE(c2%SHAPE) ) ) THEN
    ! Dim can't be less than 1
  CALL log_fatal("cube_collapse_dimension",                                    &
                 "Invalid dimension requested")
END IF

! Check the dimension is degenerate
IF ( c2%SHAPE(DIM) > 1 ) THEN
    ! Can't collapse if dimension isn't degenerate
  CALL log_fatal("cube_collapse_dimension",                                    &
                 "Input cube dimension must be degenerate")
END IF

! Make sure the cube we are copying to is deallocated
CALL cube_free(c1)

! Create a new cube, absent the degenerate dimension
c1 = cube_create([ c2%SHAPE(:DIM-1), c2%SHAPE(DIM+1:) ])

! Copy the values
c1%values(:) = c2%values(:)

RETURN

END SUBROUTINE cube_collapse_dimension
#endif
