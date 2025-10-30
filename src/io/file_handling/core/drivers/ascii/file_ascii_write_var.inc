#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_write_var_scalar(FILE, var_id, VALUE, start)

USE io_constants, ONLY: max_dim_var

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a scalar value to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, INTENT(IN) :: VALUE  ! The value to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)

! Work variables
REAL :: local_values(1)  ! An array version of value
INTEGER :: local_count(max_dim_var)  ! A version of count, which will be
                                     ! all 1s

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result
local_count(:) = 1
local_values(1) = VALUE

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, local_count)

RETURN

END SUBROUTINE file_ascii_write_var_scalar


SUBROUTINE file_ascii_write_var_1d(FILE, var_id, values, start, count_in)

USE io_constants, ONLY: mode_write

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 1d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable

! Work variables
INTEGER :: ndims  ! The number of dimensions that the variable has in file

INTEGER :: offset  ! The offset to use in the buffer


!-----------------------------------------------------------------------------

! We can't write data if the file is in define mode
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ascii_write_var_1d",                                    &
                 "Cannot write data - file is still in define mode")

! We can't write data unless we are in write mode...
IF ( FILE%mode /= mode_write )                                                 &
  CALL log_fatal("file_ascii_write_var_1d",                                    &
                 "Can only write data if file is opened in write mode")

ndims = FILE%var_ndims(var_id)

! ASCII files have the restriction that we can only write the whole variable
IF ( ANY(start(1:ndims) /= 1) )                                                &
  CALL log_fatal("file_ascii_write_var_1d",                                    &
                 "start must be 1 for all dimensions - writing part of " //    &
                 "a variable in an ASCII file is not supported")

IF ( SIZE(values) /= PRODUCT(count_in(1:ndims)) .AND.                          &
     SIZE(values) /= FILE%var_sizes(var_id) )                                  &
  CALL log_fatal("file_ascii_write_var_1d",                                    &
                 "values must have the same number of elements as the " //     &
                 "variable in file - writing part of a variable in an " //     &
                 "ASCII file is not supported")

! Now we know we can write to the buffer correctly
offset = FILE%var_offsets(var_id)
FILE%buffer(offset:(offset + FILE%var_sizes(var_id) - 1)) = values(:)

! Mark the buffer as dirty (i.e. has writes that have not been committed to file
FILE%buffer_is_dirty = .TRUE.

RETURN

END SUBROUTINE file_ascii_write_var_1d


SUBROUTINE file_ascii_write_var_2d(FILE, var_id, values, start, count_in)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 2d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable


! Work variables
REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

ALLOCATE(local_values(PRODUCT(SHAPE(values))))

local_values(:) = RESHAPE(values, [ PRODUCT(SHAPE(values)) ])

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, count_in)

DEALLOCATE(local_values)

RETURN

END SUBROUTINE file_ascii_write_var_2d


SUBROUTINE file_ascii_write_var_3d(FILE, var_id, values, start, count_in)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 3d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable


! Work variables
REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

ALLOCATE(local_values(PRODUCT(SHAPE(values))))

local_values(:) = RESHAPE(values, [ PRODUCT(SHAPE(values)) ])

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, count_in)

DEALLOCATE(local_values)

RETURN

END SUBROUTINE file_ascii_write_var_3d


SUBROUTINE file_ascii_write_var_4d(FILE, var_id, values, start, count_in)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 4d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable


! Work variables
REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

ALLOCATE(local_values(PRODUCT(SHAPE(values))))

local_values(:) = RESHAPE(values, [ PRODUCT(SHAPE(values)) ])

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, count_in)

DEALLOCATE(local_values)

RETURN

END SUBROUTINE file_ascii_write_var_4d


SUBROUTINE file_ascii_write_var_5d(FILE, var_id, values, start, count_in)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 5d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable


! Work variables
REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

ALLOCATE(local_values(PRODUCT(SHAPE(values))))

local_values(:) = RESHAPE(values, [ PRODUCT(SHAPE(values)) ])

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, count_in)

DEALLOCATE(local_values)

RETURN

END SUBROUTINE file_ascii_write_var_5d


SUBROUTINE file_ascii_write_var_6d(FILE, var_id, values, start, count_in)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 6d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable


! Work variables
REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

ALLOCATE(local_values(PRODUCT(SHAPE(values))))

local_values(:) = RESHAPE(values, [ PRODUCT(SHAPE(values)) ])

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, count_in)

DEALLOCATE(local_values)

RETURN

END SUBROUTINE file_ascii_write_var_6d


SUBROUTINE file_ascii_write_var_7d(FILE, var_id, values, start, count_in)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a 7d array to a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(:)  ! The point to start writing at
                                 ! (one value for each dimension
                                 ! of the variable in the file)
INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to write
                                    ! in each dimension of the variable


! Work variables
REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

ALLOCATE(local_values(PRODUCT(SHAPE(values))))

local_values(:) = RESHAPE(values, [ PRODUCT(SHAPE(values)) ])

CALL file_ascii_write_var_1d(FILE, var_id, local_values, start, count_in)

DEALLOCATE(local_values)

RETURN

END SUBROUTINE file_ascii_write_var_7d
#endif
