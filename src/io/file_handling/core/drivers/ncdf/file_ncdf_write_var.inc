#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_write_var_scalar(FILE, var_id, VALUE, start)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, INTENT(IN) :: VALUE  ! The value to write
INTEGER, INTENT(IN) :: start(max_dim_var)
                            ! The point to start writing at
                            ! This should contain one value for each
                            ! NON-RECORD dimension of the variable, and
                            ! any unused slots should be one

! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.

!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, VALUE, local_start)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_scalar",                            &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)

RETURN

END SUBROUTINE file_ncdf_write_var_scalar

!#############################################################################

SUBROUTINE file_ncdf_write_var_1d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.


!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_1d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_1d

!#############################################################################

SUBROUTINE file_ncdf_write_var_2d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.


!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_2d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_2d

!#############################################################################

SUBROUTINE file_ncdf_write_var_3d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.


!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_3d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_3d

!#############################################################################

SUBROUTINE file_ncdf_write_var_4d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.


!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_4d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_4d

!#############################################################################

SUBROUTINE file_ncdf_write_var_5d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.


!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_5d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_5d

!#############################################################################

SUBROUTINE file_ncdf_write_var_6d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.


!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_6d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_6d

!#############################################################################

SUBROUTINE file_ncdf_write_var_7d(FILE, var_id, values, start, counter)

USE io_constants, ONLY: max_dim_var, max_sdf_name_len

USE netcdf, ONLY: nf90_inquire_variable, nf90_put_var

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to write to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
REAL, DIMENSION(:,:,:,:,:,:,:), INTENT(IN) :: values  ! The values to write
INTEGER, INTENT(IN) :: start(max_dim_var)  ! The point to start writing
                                 ! (one value for each NON-RECORD
                                 ! dimension of the variable)
INTEGER, INTENT(IN) :: counter(max_dim_var)  ! The number of points to write
                                 ! in each dimension of the variable


! Work variables
TYPE(var_ncdf) :: var  ! Structure containing information about the
                       ! variable

INTEGER :: ndims_tot  ! The total number of dimensions that the variable
                       ! has, including record dimension

INTEGER, ALLOCATABLE :: local_start(:)
INTEGER, ALLOCATABLE :: local_count(:)

INTEGER :: ERROR, error_name  ! Error indicators

CHARACTER(LEN=max_sdf_name_len) :: var_name  ! Name of variable.

!-----------------------------------------------------------------------------

! Copy values into a local variable for convenience
var = FILE%vars(var_id)

ndims_tot = var%ndims
IF ( var%is_record ) ndims_tot = ndims_tot + 1

! Set up the local_start array - this consists of one value for each
! dimension of the variable from the given start array, plus a value so that
! we write to the correct record, if required
ALLOCATE(local_start(ndims_tot))
local_start(1:var%ndims) = start(1:var%ndims)
IF ( var%is_record ) local_start(ndims_tot) = FILE%current_record

! Set up the local_count array - this consists of one value for each
! dimension of the variable from the given counter array, plus a value so that
! we only write to the correct record, if required
ALLOCATE(local_count(ndims_tot))
local_count(1:var%ndims) = counter(1:var%ndims)
IF ( var%is_record ) local_count(ndims_tot) = 1

! Try to write the variable
ERROR = nf90_put_var(FILE%id, var%id, values, local_start, local_count)
IF ( ERROR /= nf90_noerr ) THEN
  !   Get variable name.
  error_name = nf90_inquire_variable(FILE%id, var%id, NAME = var_name )
  IF ( error_name /= nf90_noerr ) var_name = 'unknown'
  CALL log_fatal_ncdf("file_ncdf_write_var_7d",                                &
                      TRIM(FILE%NAME) // ": " //                               &
                      "Error writing variable '" // TRIM(var_name) // "'",     &
                       ERROR)
END IF

DEALLOCATE(local_start)
DEALLOCATE(local_count)

RETURN

END SUBROUTINE file_ncdf_write_var_7d
#endif
