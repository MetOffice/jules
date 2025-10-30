#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!*****************************************************************************
! Note that these operators are all 'mdi-propagating', i.e. any values set
! to mdi in incoming cubes will propagate into outgoing cubes as mdi
!*****************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION cube_min(c1, c2) RESULT(c3)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two cubes and finds the minimum values element-wise
!   If the cubes do not have the same shape, an error is thrown
!
! Code Author: Karina Williams
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: c1, c2  ! The cubes to compare


! Return type
TYPE(data_cube) :: c3  ! The cube containing the minimum values


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the two input cubes have compatible shapes
!-----------------------------------------------------------------------------
IF ( SIZE(c1%SHAPE) /= SIZE(c2%SHAPE) .OR. .NOT. ALL(c1%SHAPE == c2%SHAPE) )   &
  CALL log_fatal("min_of_cube_and_cube",                                       &
                 "Error finding min of cubes - shapes do not match")

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c3 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves mdis from the
! incoming cubes
!-----------------------------------------------------------------------------
c3%values = mdi_safe_min(c1%values, c2%values)

RETURN

END FUNCTION cube_min


!*****************************************************************************


ELEMENTAL FUNCTION mdi_safe_min(x, y) RESULT(z)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two values and finds minimum in a mdi-preserving way
!   This function is declared as ELEMENTAL, meaning that it can operate on
!   arrays
!
! Code Author: Karina Williams
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: x, y  ! The values to find the minimum of

! Return type
REAL :: z


!-----------------------------------------------------------------------------


! If either of the incoming values is mdi, then the result is mdi
IF ( is_mdi(x) .OR. is_mdi(y) ) THEN
  z = mdi
  RETURN
END IF

! Otherwise, just find the minimum
z = MIN(x, y)

RETURN

END FUNCTION mdi_safe_min


!-----------------------------------------------------------------------------


FUNCTION cube_max(c1, c2) RESULT(c3)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two cubes and finds the maximum values element-wise
!   If the cubes do not have the same shape, an error is thrown
!
! Code Author: Karina Williams
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: c1, c2  ! The cubes to compare


! Return type
TYPE(data_cube) :: c3  ! The cube containing the maximum values


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the two input cubes have compatible shapes
!-----------------------------------------------------------------------------
IF ( SIZE(c1%SHAPE) /= SIZE(c2%SHAPE) .OR. .NOT. ALL(c1%SHAPE == c2%SHAPE) )   &
  CALL log_fatal("max_of_cube_and_cube",                                       &
                 "Error finding max of cubes - shapes do not match")

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c3 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves mdis from the
! incoming cubes
!-----------------------------------------------------------------------------
c3%values = mdi_safe_max(c1%values, c2%values)

RETURN

END FUNCTION cube_max


!*****************************************************************************


ELEMENTAL FUNCTION mdi_safe_max(x, y) RESULT(z)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two values and finds maximum in a mdi-preserving way
!   This function is declared as ELEMENTAL, meaning that it can operate on
!   arrays
!
! Code Author: Karina Williams
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: x, y  ! The values to find the maximum of

! Return type
REAL :: z


!-----------------------------------------------------------------------------


! If either of the incoming values is mdi, then the result is mdi
IF ( is_mdi(x) .OR. is_mdi(y) ) THEN
  z = mdi
  RETURN
END IF

! Otherwise, just find the maximum
z = MAX(x, y)

RETURN

END FUNCTION mdi_safe_max

#endif
