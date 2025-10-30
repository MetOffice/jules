#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ts_def_attr_real(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: attr_global

USE dictionary_mod, ONLY: dict_create, dict_set

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
TYPE(file_ts), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
REAL, INTENT(IN) :: VALUE         ! The value of the attribute


!-----------------------------------------------------------------------------


! If we are not in define mode, error out
IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ts_def_attr_real",                                      &
                 "Cannot define attribute - file is not in define mode")

!-----------------------------------------------------------------------------
! Work out what dictionary we want to populate
!-----------------------------------------------------------------------------
IF ( var_id == attr_global ) THEN
  ! If it is a global attribute, use the global dictionary
  ! Create the required dictionary if it has not been used yet
  IF ( FILE%attrs_real%length == 0 )                                           &
    FILE%attrs_real = dict_create(max_attr_file, 1.0)

  CALL dict_set(FILE%attrs_real, NAME, VALUE)
ELSE
  ! Otherwise, use the dictionary on the specified variable
  ! Create the required dictionary if it has not been used yet
  IF ( FILE%vars(var_id)%attrs_real%length == 0 )                              &
    FILE%vars(var_id)%attrs_real = dict_create(max_attr_file, 1.0)

  CALL dict_set(FILE%vars(var_id)%attrs_real, NAME, VALUE)
END IF

RETURN

END SUBROUTINE file_ts_def_attr_real


SUBROUTINE file_ts_def_attr_int(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: attr_global

USE dictionary_mod, ONLY: dict_create, dict_set

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
TYPE(file_ts), INTENT(IN OUT) :: FILE
                                  ! The file to define the attribute in
INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                  ! attribute on
CHARACTER(LEN=*), INTENT(IN) :: NAME
                                  ! The name of the attribute
INTEGER, INTENT(IN) :: VALUE      ! The value of the attribute


!-----------------------------------------------------------------------------

! If we are not in define mode, error out
IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ts_def_attr_int",                                       &
                 "Cannot define attribute - file is not in define mode")

!-----------------------------------------------------------------------------
! Work out what dictionary we want to populate
!-----------------------------------------------------------------------------
IF ( var_id == attr_global ) THEN
  ! If it is a global attribute, use the global dictionary
  ! Create the required dictionary if it has not been used yet
  IF ( FILE%attrs_int%length == 0 )                                            &
    FILE%attrs_int = dict_create(max_attr_file, INT(1))

  CALL dict_set(FILE%attrs_int, NAME, VALUE)
ELSE
  ! Otherwise, use the dictionary on the specified variable
  ! Create the required dictionary if it has not been used yet
  IF ( FILE%vars(var_id)%attrs_int%length == 0 )                               &
    FILE%vars(var_id)%attrs_int = dict_create(max_attr_file, INT(1))

  CALL dict_set(FILE%vars(var_id)%attrs_int, NAME, VALUE)
END IF

RETURN

END SUBROUTINE file_ts_def_attr_int


SUBROUTINE file_ts_def_attr_char(FILE, var_id, NAME, VALUE)

USE io_constants, ONLY: attr_global

USE dictionary_mod, ONLY: dict_create, dict_set

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
TYPE(file_ts), INTENT(IN OUT) :: FILE
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
  CALL log_fatal("file_ts_def_attr_char",                                      &
                 "Cannot define attribute - file is not in define mode")

!-----------------------------------------------------------------------------
! Work out what dictionary we want to populate
!-----------------------------------------------------------------------------
IF ( var_id == attr_global ) THEN
  ! If it is a global attribute, use the global dictionary
  ! Create the required dictionary if it has not been used yet
  IF ( FILE%attrs_char%length == 0 )                                           &
    FILE%attrs_char = dict_create(max_attr_file, NAME)

  CALL dict_set(FILE%attrs_char, NAME, VALUE)
ELSE
  ! Otherwise, use the dictionary on the specified variable
  ! Create the required dictionary if it has not been used yet
  IF ( FILE%vars(var_id)%attrs_char%length == 0 )                              &
    FILE%vars(var_id)%attrs_char = dict_create(max_attr_file, NAME)

  CALL dict_set(FILE%vars(var_id)%attrs_char, NAME, VALUE)
END IF

RETURN

END SUBROUTINE file_ts_def_attr_char
#endif
