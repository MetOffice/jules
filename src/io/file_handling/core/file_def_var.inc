#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_def_var(FILE, var_name, dims, is_record) RESULT(var_id)

USE io_constants, ONLY: max_sdf_name_len

USE driver_ascii_mod, ONLY: file_ascii_def_var
USE driver_ncdf_mod, ONLY: file_ncdf_def_var

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a variable in the given file, returning the variable id
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_handle), INTENT(IN OUT) :: FILE
                                ! The file to define the variable in
CHARACTER(LEN=*), INTENT(IN) :: var_name
                                  ! The name of the variable
INTEGER, INTENT(IN) :: dims(:)    ! The ids of the NON-RECORD dimensions of
                                  ! the variable
LOGICAL, INTENT(IN) :: is_record  ! Indicates whether the variable uses the
                                  ! record dimension

! Return type
INTEGER :: var_id               ! The variable id


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  var_id = file_ascii_def_var(FILE%ascii, var_name, dims, is_record)

CASE ( driver_ncdf )
  var_id = file_ncdf_def_var(FILE%ncdf, var_name, dims, is_record)

CASE DEFAULT
  CALL log_fatal("file_def_var",                                               &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END FUNCTION file_def_var
#endif
