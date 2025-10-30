#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_gridded_def_attr_real(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: max_sdf_name_len, attr_global

USE file_mod, ONLY: file_def_attr

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
TYPE(file_gridded), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
REAL, INTENT(IN) :: VALUE         ! The value of the attribute

! Work variables
INTEGER :: var_id_local


!-----------------------------------------------------------------------------

! Look up the variable id in the underlying file, unless a global attribute
! has been requested
! var_id is an index in the var_ids array of the file_gridded object, as
! returned by file_gridded_def_var
var_id_local = var_id
IF ( var_id /= attr_global ) var_id_local = FILE%vars(var_id)%id

! Now just defer to the underlying file
CALL file_def_attr(FILE%fh, var_id_local, NAME, VALUE)

RETURN

END SUBROUTINE file_gridded_def_attr_real


SUBROUTINE file_gridded_def_attr_int(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: max_sdf_name_len, attr_global

USE file_mod, ONLY: file_def_attr

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
TYPE(file_gridded), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
INTEGER, INTENT(IN) :: VALUE      ! The value of the attribute

! Work variables
INTEGER :: var_id_local


!-----------------------------------------------------------------------------

! Look up the variable id in the underlying file, unless a global attribute
! has been requested
! var_id is an index in the var_ids array of the file_gridded object, as
! returned by file_gridded_def_var
var_id_local = var_id
IF ( var_id /= attr_global ) var_id_local = FILE%vars(var_id)%id

! Now just defer to the underlying file
CALL file_def_attr(FILE%fh, var_id_local, NAME, VALUE)

RETURN

END SUBROUTINE file_gridded_def_attr_int


SUBROUTINE file_gridded_def_attr_char(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: max_sdf_name_len, attr_global

USE file_mod, ONLY: file_def_attr

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
TYPE(file_gridded), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
CHARACTER(LEN=*), INTENT(IN) :: VALUE
                                  ! The value of the attribute

! Work variables
INTEGER :: var_id_local


!-----------------------------------------------------------------------------

! Look up the variable id in the underlying file, unless a global attribute
! has been requested
! var_id is an index in the var_ids array of the file_gridded object, as
! returned by file_gridded_def_var
var_id_local = var_id
IF ( var_id /= attr_global ) var_id_local = FILE%vars(var_id)%id

! Now just defer to the underlying file
CALL file_def_attr(FILE%fh, var_id_local, NAME, VALUE)

RETURN

END SUBROUTINE file_gridded_def_attr_char
#endif
