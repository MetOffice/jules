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

PURE LOGICAL FUNCTION is_mdi(x)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks if a value is (epsilon close to) the missing data value (mdi).
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
REAL, INTENT(IN) :: x
REAL, PARAMETER :: small_val = EPSILON(1.0)

!-----------------------------------------------------------------------------

! Return TRUE if the result is epsilon close to mdi, otherwise return false
is_mdi = ABS(x - mdi) < small_val

RETURN
END FUNCTION is_mdi

!##############################################################################

FUNCTION cube_add_scalar(c1, add) RESULT(c2)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a cube and adds a scalar to its contents
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1  ! The cube to add to
REAL, INTENT(IN) :: add  ! The scalar to add

! Return type
TYPE(data_cube) :: c2  ! The cube containing the summed data

!-----------------------------------------------------------------------------
! Create a new cube of the correct shape
!-----------------------------------------------------------------------------
c2 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves missing data
! values from the incoming cubes.
!-----------------------------------------------------------------------------
c2%values = mdi_safe_add(c1%values, add)

RETURN
END FUNCTION cube_add_scalar

!##############################################################################

FUNCTION cube_add_cube(c1, c2) RESULT(c3)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two cubes and adds their contents together element-wise
!   If the cubes do not have the same shape, an error is thrown
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1, c2  ! The cubes to add

! Return type
TYPE(data_cube) :: c3  ! The cube containing the summed data

!-----------------------------------------------------------------------------
! Check that the two cubes are of the same shape.
!-----------------------------------------------------------------------------
IF ( SIZE(c1%SHAPE) /= SIZE(c2%SHAPE) ) THEN
  CALL log_fatal("cube_add_cube",                                              &
                 "Error adding cubes - array ranks do not match")
END IF

IF ( .NOT. ALL(c1%SHAPE == c2%SHAPE) ) THEN
  CALL log_fatal("cube_add_cube",                                              &
                 "Error adding cubes - shapes do not match")
END IF

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c3 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves missing data
! values from the incoming cubes.
!-----------------------------------------------------------------------------
c3%values = mdi_safe_add(c1%values, c2%values)

RETURN
END FUNCTION cube_add_cube

!##############################################################################

ELEMENTAL FUNCTION mdi_safe_add(x, y) RESULT(z)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two values and adds them together in a mdi-preserving way
!   This function is declared as ELEMENTAL, meaning that it can operate on
!   arrays
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
REAL, INTENT(IN) :: x, y  ! The values to add together

! Return type
REAL :: z

!-----------------------------------------------------------------------------

! If either of the incoming values is the missing data value, the result is
! the missing data value.
IF ( is_mdi(x) .OR. is_mdi(y) ) THEN
  z = mdi
  RETURN
END IF

! Otherwise, just add the values together to produce the result
z = x + y

RETURN
END FUNCTION mdi_safe_add

!##############################################################################

FUNCTION cube_sub_scalar(c1, sub) RESULT(c2)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a cube and subtracts a scalar from its contents
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1  ! The cube to subtract from
REAL, INTENT(IN) :: sub  ! The scalar to subtract

! Return type
TYPE(data_cube) :: c2  ! The cube containing the subtracted data

!-----------------------------------------------------------------------------
! Create a new cube of the correct shape
!-----------------------------------------------------------------------------
c2 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves missing data
! values from the incoming cubes.
!-----------------------------------------------------------------------------
c2%values = mdi_safe_sub(c1%values, sub)

RETURN
END FUNCTION cube_sub_scalar

!##############################################################################

FUNCTION cube_sub_cube(c1, c2) RESULT(c3)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two cubes and subtracts the contents of the second from the first,
!   element-wise.
!   If the cubes do not have the same shape, an error is thrown.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1, c2  ! The cubes to difference

! Return type
TYPE(data_cube) :: c3  ! The cube containing the differenced data

!-----------------------------------------------------------------------------
! Check that the two cubes are of the same shape.
!-----------------------------------------------------------------------------
IF ( SIZE(c1%SHAPE) /= SIZE(c2%SHAPE) ) THEN
  CALL log_fatal("cube_sub_cube",                                              &
                 "Error subtracting cubes - array ranks do not match")
END IF

IF ( .NOT. ALL(c1%SHAPE == c2%SHAPE) ) THEN
  CALL log_fatal("cube_sub_cube",                                              &
                 "Error subtracting cubes - shapes do not match")
END IF

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c3 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves missing data
! values from the incoming cubes.
!-----------------------------------------------------------------------------
c3%values = mdi_safe_sub(c1%values, c2%values)

RETURN
END FUNCTION cube_sub_cube

!##############################################################################

ELEMENTAL FUNCTION mdi_safe_sub(x, y) RESULT(z)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two values x and y and subtracts y from x in a mdi-preserving way.
!   This function is declared as ELEMENTAL, meaning that it can operate on
!   arrays.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
REAL, INTENT(IN) :: x, y  ! The values to subtract

! Return type
REAL :: z

!-----------------------------------------------------------------------------

! If either of the incoming values is the missing data value, the result is
! the missing data value.
IF ( is_mdi(x) .OR. is_mdi(y) ) THEN
  z = mdi
  RETURN
END IF

! Otherwise, just subtract y from x to produce the result
z = x - y

RETURN
END FUNCTION mdi_safe_sub

!##############################################################################

FUNCTION cube_mul_scalar(c1, mult) RESULT(c2)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a cube and multiplies its contents by a scalar
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1  ! The cube to multiply
REAL, INTENT(IN) :: mult  ! The scalar to multiply by

! Return type
TYPE(data_cube) :: c2  ! The cube containing the multiplied data

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c2 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves mdis from the
! incoming cubes
!-----------------------------------------------------------------------------
c2%values = mdi_safe_mul(c1%values, mult)

RETURN

END FUNCTION cube_mul_scalar

!##############################################################################

FUNCTION cube_mul_cube(c1, c2) RESULT(c3)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two cubes and multiplies their contents together element-wise
!   If the cubes do not have the same shape, an error is thrown
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1, c2  ! The cubes to multiply

! Return type
TYPE(data_cube) :: c3  ! The cube containing the multiplied data

!-----------------------------------------------------------------------------
! Check that the two cubes are of the same shape.
!-----------------------------------------------------------------------------
IF ( SIZE(c1%SHAPE) /= SIZE(c2%SHAPE) ) THEN
  CALL log_fatal("cube_mul_cube",                                              &
                 "Error multiplying cubes - array ranks do not match")
END IF

IF ( .NOT. ALL(c1%SHAPE == c2%SHAPE) ) THEN
  CALL log_fatal("cube_mul_cube",                                              &
                 "Error multiplying cubes - shapes do not match")
END IF

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c3 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves missing data
! values from the incoming cubes.
!-----------------------------------------------------------------------------
c3%values = mdi_safe_mul(c1%values, c2%values)

RETURN
END FUNCTION cube_mul_cube

!##############################################################################

ELEMENTAL FUNCTION mdi_safe_mul(x, y) RESULT(z)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two values and multiplies them in a mdi-preserving way
!   This function is declared as ELEMENTAL, meaning that it can operate on
!   arrays
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
REAL, INTENT(IN) :: x, y  ! The values to multiply

! Return type
REAL :: z

!-----------------------------------------------------------------------------

! If either of the incoming values is the missing data value, the result is
! the missing data value.
IF ( is_mdi(x) .OR. is_mdi(y) ) THEN
  z = mdi
  RETURN
END IF

! Otherwise, just subtract y from x to produce the result
z = x * y

RETURN
END FUNCTION mdi_safe_mul

!##############################################################################

FUNCTION cube_div_scalar(c1, divisor) RESULT(c2)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a cube and divides its contents by a scalar
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1  ! The cube to divide
REAL, INTENT(IN) :: divisor  ! The scalar to divide by

! Return type
TYPE(data_cube) :: c2  ! The cube containing the divided data

!-----------------------------------------------------------------------------
! Create a cube of the correct shape
!-----------------------------------------------------------------------------
c2 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Set the data for the outgoing cube in a way that preserves missing data
! values from the incoming cubes.
!-----------------------------------------------------------------------------
c2%values = mdi_safe_div(c1%values, divisor)

RETURN
END FUNCTION cube_div_scalar

!##############################################################################

FUNCTION cube_div_cube(c1, c2) RESULT(c3)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two cubes and divides the contents of the first by the contents of
!   the second, element-wise
!   Where the second cube has near-zero values, the resulting cube will contain
!   zeroes
!   If the cubes do not have the same shape, an error is thrown
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
TYPE(data_cube), INTENT(IN) :: c1, c2  ! The cubes to divide

! Return type
TYPE(data_cube) :: c3  ! The cube containing the divided data

!-----------------------------------------------------------------------------
! Check that the two cubes are of the same shape.
!-----------------------------------------------------------------------------
IF ( SIZE(c1%SHAPE) /= SIZE(c2%SHAPE) ) THEN
  CALL log_fatal("cube_div_cube",                                              &
                 "Error dividing cubes - array ranks do not match")
END IF

IF ( .NOT. ALL(c1%SHAPE == c2%SHAPE) ) THEN
  CALL log_fatal("cube_div_cube",                                              &
                 "Error dividing cubes - shapes do not match")
END IF

!-----------------------------------------------------------------------------
! Create a cube of the correct shape.
!-----------------------------------------------------------------------------
c3 = cube_create(c1%SHAPE)

!-----------------------------------------------------------------------------
! Create a cube containing divided data.
! Set the data for the outgoing cube in a way that preserves mdis from the
! incoming cubes.
!-----------------------------------------------------------------------------
c3%values = mdi_safe_div(c1%values, c2%values)

RETURN
END FUNCTION cube_div_cube

!##############################################################################

ELEMENTAL FUNCTION mdi_safe_div(x, y) RESULT(z)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two values x and y and divides x by y in a mdi-preserving way
!   This function is declared as ELEMENTAL, meaning that it can operate on
!   arrays
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 95.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Argument types
REAL, INTENT(IN) :: x, y  ! The values to divide

! Return type
REAL :: z

!-----------------------------------------------------------------------------

! If either of the incoming values is the missing data value, the result is
! the missing data value.
IF ( is_mdi(x) .OR. is_mdi(y) ) THEN
  z = mdi
  RETURN
END IF

! If the denominator is 0, return 0
IF ( ABS(y) < EPSILON(1.0) ) THEN
  z = 0
  RETURN
END IF

! Otherwise, divide x by y to produce the result
z = x / y

RETURN
END FUNCTION mdi_safe_div

!##############################################################################

#endif
