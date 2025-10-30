#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_def_attr_real(FILE, var_id, NAME, VALUE)

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
TYPE(file_ascii), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
REAL, INTENT(IN) :: VALUE         ! The value of the attribute


!-----------------------------------------------------------------------------

! Since all attributes on ASCII files will eventually be char when written
! to file, we just delegate to def_attr_char with a converted value
CALL file_ascii_def_attr_char(FILE, var_id, NAME, TRIM(to_string(VALUE)))

RETURN

END SUBROUTINE file_ascii_def_attr_real


SUBROUTINE file_ascii_def_attr_int(FILE, var_id, NAME, VALUE)

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
TYPE(file_ascii), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
INTEGER, INTENT(IN) :: VALUE      ! The value of the attribute


!-----------------------------------------------------------------------------

! Since all attributes on ASCII files will eventually be char when written
! to file, we just delegate to def_attr_char with a converted value
CALL file_ascii_def_attr_char(FILE, var_id, NAME, TRIM(to_string(VALUE)))

RETURN

END SUBROUTINE file_ascii_def_attr_int


SUBROUTINE file_ascii_def_attr_char(FILE, var_id, NAME, VALUE)

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
TYPE(file_ascii), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
CHARACTER(LEN=*), INTENT(IN) :: VALUE
                                  ! The value of the attribute


!-----------------------------------------------------------------------------

! If we are not in define mode, error out
IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ascii_def_attr_char",                                   &
                 "Cannot define attribute - file is not in define mode")

! If adding another attribute will cause us to have too many attributes,
! error out
IF ( FILE%nattrs >= max_attr_file )                                            &
  CALL log_fatal("file_ascii_def_attr_char",                                   &
                 "Too many attributes in file - try increasing max_attr_file")

!-----------------------------------------------------------------------------
! Store information about the attribute so that it can be used later
!-----------------------------------------------------------------------------
FILE%nattrs = FILE%nattrs + 1

FILE%attr_var_ids(FILE%nattrs) = var_id
! We trim trailing spaces from name and leading spaces from value so that
! they will be the correct distance apart. Then left align the whole lot
FILE%attr_values(FILE%nattrs)  = ADJUSTL(TRIM(NAME) // " = " // ADJUSTL(VALUE))

RETURN

END SUBROUTINE file_ascii_def_attr_char
#endif
