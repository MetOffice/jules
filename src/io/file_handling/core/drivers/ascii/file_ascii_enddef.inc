#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_enddef(FILE)

USE io_constants, ONLY: mode_read, mode_write, attr_global

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes the file out of definition mode - no more dimensions or variables
!   may be defined after this
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE

! Work variables
CHARACTER(LEN=100) :: var_dims_str  ! Used when writing headers to store
                                    ! variables dimension names as a
                                    ! string for writing

INTEGER :: buffer_size  ! The size of the buffer
INTEGER :: offset  ! Used in calculation of offsets
INTEGER :: ERROR  ! Error indicator

INTEGER :: i,j  ! Loop counters


!-----------------------------------------------------------------------------

FILE%define_mode = .FALSE.

!-----------------------------------------------------------------------------
! Allocate the buffer - it must be big enough to hold one records worth of
! data for each variable
!-----------------------------------------------------------------------------
! To get the size of the buffer, just sum the sizes of the defined variables
buffer_size = SUM(FILE%var_sizes(1:FILE%nvars))
ALLOCATE(FILE%buffer(buffer_size), STAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("file_ascii_enddef",                                          &
                 "Error allocating memory - " //                               &
                 "(STAT=" // TRIM(to_string(ERROR)) // ")")

! Now we know the size of the buffer, we can make a better guess at the
! record length we will need. For now, we'll assume no more than 30 characters
! per buffer element - this should be very conservative
FILE%record_len = buffer_size * 30

!-----------------------------------------------------------------------------
! Calculate offsets for each variable
! It is assumed that they appear in the file in the order in which they
! were defined
!-----------------------------------------------------------------------------
offset = 1
DO i = 1,FILE%nvars
  FILE%var_offsets(i) = offset
  offset = offset + FILE%var_sizes(i)
END DO


! Now we do different things depending on mode
SELECT CASE ( FILE%mode )
CASE ( mode_read )
  ! In read mode, we fill the buffer with the first data so we are ready to read
  CALL file_ascii_fill_buffer(FILE)

CASE ( mode_write )
  !-----------------------------------------------------------------------------
  ! In write mode, we write the headers describing the variables in the file
  !
  ! The header is of the form:
  !
  ! #
  ! # CREATED BY JULES LSM
  ! #
  ! # Global attributes:
  ! #     global_attr_1 = foobar ;
  ! #     global_attr_2 = baz ;
  ! #
  ! # Dimensions:
  ! #     dim1 = size1 ;
  ! #     dim2 = size2 ;
  ! #     dim3 = size3 ;
  ! #     dim4 = UNLIMITED ;
  ! #
  ! # Variables:
  ! #     variable1(dim1,dim2) ;
  ! #         variable1_attr_1 = foobaz ;
  ! #         variable1_attr_2 = blah ;
  ! #
  ! #     variable2(dim1) ;
  ! #
  ! #     variable3(dim1,dim2,dim3) ;
  ! #
  !-----------------------------------------------------------------------------
  ! First write the created by header
  WRITE(FILE%UNIT, "(A)") "#"
  WRITE(FILE%UNIT, "(A)") "# CREATED BY JULES LSM"
  WRITE(FILE%UNIT, "(A)") "#"

  !-----------------------------------------------------------------------------
  ! Write global attributes
  !-----------------------------------------------------------------------------
  WRITE(FILE%UNIT, "(A)") "# Global attributes:"
  DO i = 1,FILE%nattrs
    IF ( FILE%attr_var_ids(i) == attr_global )                                 &
      WRITE(FILE%UNIT, "(A)") "#     " // TRIM(FILE%attr_values(i)) // " ;"
  END DO

  WRITE(FILE%UNIT, "(A)") "#"

  !-----------------------------------------------------------------------------
  ! Write information about the dimensions in the file
  !-----------------------------------------------------------------------------
  WRITE(FILE%UNIT, "(A)") "# Dimensions:"
  DO i = 1,FILE%ndims
    WRITE(FILE%UNIT, "(A)") "#     " // TRIM(FILE%dim_names(i)) //             &
                            " = " // TRIM(to_string(FILE%dim_sizes(i))) // " ;"

  END DO
  ! Write information about the record dimension
  IF ( FILE%has_record_dim )                                                   &
    WRITE(FILE%UNIT, "(A)") "#     " // TRIM(FILE%record_dim_name) //          &
                            " = UNLIMITED ;"

  WRITE(FILE%UNIT, "(A)") "#"

  !-----------------------------------------------------------------------------
  ! Write information about the variables in the file
  !-----------------------------------------------------------------------------
  WRITE(FILE%UNIT, "(A)") "# Variables:"
  DO i = 1,FILE%nvars
    ! Build the dimension names string for the variable
    var_dims_str = ""
    DO j = 1,FILE%var_ndims(i)
      var_dims_str = TRIM(var_dims_str) // FILE%dim_names( FILE%var_dims(i,j) )

      ! Add a comma unless this is the last dimension that the variable has, including
      ! the addition of the record dim below
      IF ( j < FILE%var_ndims(i) .OR. FILE%has_record_dim )                    &
        var_dims_str = TRIM(var_dims_str) // ","
    END DO
    ! Add the record dimension if it is in use
    IF ( FILE%has_record_dim ) THEN
      var_dims_str = TRIM(var_dims_str) // FILE%record_dim_name
    END IF

    WRITE(FILE%UNIT, "(A)") "#     " // TRIM(FILE%var_names(i)) //             &
                            "(" // TRIM(var_dims_str) // ") ;"

    ! Write info about each of the variables attributes
    DO j = 1,FILE%nattrs
      IF ( FILE%attr_var_ids(j) == i )                                         &
        WRITE(FILE%UNIT, "(A)") "#         " // TRIM(FILE%attr_values(j)) // " ;"
    END DO

    WRITE(FILE%UNIT, "(A)") "#"
  END DO

  ! Set the format string to use for output, now we know how many elements are in
  ! the buffer
  out_format_str = "(" // TRIM(to_string(buffer_size)) // "G15.7e2)"

END SELECT

RETURN

END SUBROUTINE file_ascii_enddef
#endif
