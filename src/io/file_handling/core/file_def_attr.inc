#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_def_attr_real(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: max_sdf_name_len

USE driver_ascii_mod, ONLY: file_ascii_def_attr_real
USE driver_ncdf_mod, ONLY: file_ncdf_def_attr_real

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Define a real valued attribute on the given variable with the given name
!   and value
!   To define a global attribute, specify attr_global as var_id
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
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
REAL, INTENT(IN) :: VALUE         ! The value of the attribute


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_def_attr_real(FILE%ascii, var_id, NAME, VALUE)

CASE ( driver_ncdf )
  CALL file_ncdf_def_attr_real(FILE%ncdf, var_id, NAME, VALUE)

CASE DEFAULT
  CALL log_fatal("file_def_attr_real",                                         &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_def_attr_real


SUBROUTINE file_def_attr_int(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: max_sdf_name_len

USE driver_ascii_mod, ONLY: file_ascii_def_attr_int
USE driver_ncdf_mod, ONLY: file_ncdf_def_attr_int

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Define an integer valued attribute on the given variable with the given
!   name and value
!   To define a global attribute, specify attr_global as var_id
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
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
INTEGER, INTENT(IN) :: VALUE      ! The value of the attribute


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_def_attr_int(FILE%ascii, var_id, NAME, VALUE)

CASE ( driver_ncdf )
  CALL file_ncdf_def_attr_int(FILE%ncdf, var_id, NAME, VALUE)

CASE DEFAULT
  CALL log_fatal("file_def_attr_int",                                          &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_def_attr_int


SUBROUTINE file_def_attr_char(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: max_sdf_name_len

USE driver_ascii_mod, ONLY: file_ascii_def_attr_char
USE driver_ncdf_mod, ONLY: file_ncdf_def_attr_char

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Define a character valued attribute on the given variable with the given
!   name and value
!   To define a global attribute, specify attr_global as var_id
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
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
CHARACTER(LEN=*), INTENT(IN) :: VALUE
                                  ! The value of the attribute


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_def_attr_char(FILE%ascii, var_id, NAME, VALUE)

CASE ( driver_ncdf )
  CALL file_ncdf_def_attr_char(FILE%ncdf, var_id, NAME, VALUE)

CASE DEFAULT
  CALL log_fatal("file_def_attr_char",                                         &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_def_attr_char
#endif
