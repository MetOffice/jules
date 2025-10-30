#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE dictionary_mod

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: dict_key_len = 50
INTEGER, PARAMETER :: dict_char_val_len = 100


!-----------------------------------------------------------------------------
! Module types
!-----------------------------------------------------------------------------
TYPE :: dict

  INTEGER :: length = 0  ! The number of elements currently in the dictionary

  CHARACTER(LEN=dict_key_len), POINTER :: keys(:) => NULL()

  ! Only one of these arrays will be required in any given dictionary
  ! Which one is required will be decided in dict_create, and will be allocated
  ! to have size DICT_MAX_LEN
  INTEGER, POINTER :: int_values(:) => NULL()
  REAL, POINTER :: real_values(:) => NULL()
  CHARACTER(LEN=dict_char_val_len), POINTER :: char_values(:) => NULL()

END TYPE dict


!-----------------------------------------------------------------------------
! Overloads for generic procedures
!-----------------------------------------------------------------------------
INTERFACE dict_create
  MODULE PROCEDURE dict_create_int, dict_create_real, dict_create_char
END INTERFACE dict_create

INTERFACE dict_get
  MODULE PROCEDURE dict_get_int, dict_get_real, dict_get_char
END INTERFACE dict_get

INTERFACE dict_set
  MODULE PROCEDURE dict_set_int, dict_set_real, dict_set_char
END INTERFACE dict_set


CONTAINS


!-----------------------------------------------------------------------------
! Dictionary creation routines for each type
! Each routine takes a single argument that just indicates the type to use
! The appropriate procedure to use is selected by the interface definition
!-----------------------------------------------------------------------------
FUNCTION dict_create_int(max_len, type_indicator) RESULT(dict_val)

IMPLICIT NONE

! Argument types
INTEGER, INTENT(IN) :: max_len  ! The maximum number of entries that
                                ! the dictionary can hold
INTEGER, INTENT(IN) :: type_indicator

! Return type
TYPE(dict) :: dict_val

!-----------------------------------------------------------------------------

! This dictionary will be using the int_values component
ALLOCATE(dict_val%keys(max_len))
ALLOCATE(dict_val%int_values(max_len))

RETURN

END FUNCTION dict_create_int


FUNCTION dict_create_real(max_len, type_indicator) RESULT(dict_val)

IMPLICIT NONE

! Argument types
INTEGER, INTENT(IN) :: max_len  ! The maximum number of entries that
                                ! the dictionary can hold
REAL, INTENT(IN) :: type_indicator

! Return type
TYPE(dict) :: dict_val

!-----------------------------------------------------------------------------

! This dictionary will be using the real_values component
ALLOCATE(dict_val%keys(max_len))
ALLOCATE(dict_val%real_values(max_len))

RETURN

END FUNCTION dict_create_real


FUNCTION dict_create_char(max_len, type_indicator) RESULT(dict_val)

IMPLICIT NONE

! Argument types
INTEGER, INTENT(IN) :: max_len  ! The maximum number of entries that
                                ! the dictionary can hold
CHARACTER(LEN=*), INTENT(IN) :: type_indicator

! Return type
TYPE(dict) :: dict_val

!-----------------------------------------------------------------------------

! This dictionary will be using the char_values component
ALLOCATE(dict_val%keys(max_len))
ALLOCATE(dict_val%char_values(max_len))

RETURN

END FUNCTION dict_create_char


!-----------------------------------------------------------------------------
! Routines for getting a value from the dictionary by key
!-----------------------------------------------------------------------------
SUBROUTINE dict_get_int(dict_val, key, VALUE)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN) :: dict_val  ! The dictionary to retrieve to value from
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to return the value for
INTEGER, INTENT(OUT) :: VALUE  ! The value associated with the key

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------

! Check the correct internal array is allocated
IF ( .NOT. ASSOCIATED(dict_val%int_values) )                                   &
  CALL log_fatal("dict_get_int",                                               &
                 "Dictionary is not of integer type")

! Try to locate the given key
DO i = 1,dict_val%length
  IF ( dict_val%keys(i) == key ) THEN
    VALUE = dict_val%int_values(i)
    RETURN
  END IF
END DO

! If we get to here, the key does not exist
CALL log_fatal("dict_get_int",                                                 &
               "Dictionary does not contain key '" // TRIM(key) // "'")

RETURN

END SUBROUTINE dict_get_int


SUBROUTINE dict_get_real(dict_val, key, VALUE)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN) :: dict_val  ! The dictionary to retrieve to value from
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to return the value for
REAL, INTENT(OUT) :: VALUE  ! The value associated with the key

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------

! Check the correct internal array is allocated
IF ( .NOT. ASSOCIATED(dict_val%real_values) )                                  &
  CALL log_fatal("dict_get_real",                                              &
                 "Dictionary is not of real type")

! Try to locate the given key
DO i = 1,dict_val%length
  IF ( dict_val%keys(i) == key ) THEN
    VALUE = dict_val%real_values(i)
    RETURN
  END IF
END DO

! If we get to here, the key does not exist
CALL log_fatal("dict_get_real",                                                &
               "Dictionary does not contain key '" // TRIM(key) // "'")

RETURN

END SUBROUTINE dict_get_real


SUBROUTINE dict_get_char(dict_val, key, VALUE)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN) :: dict_val  ! The dictionary to retrieve to value from
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to return the value for
CHARACTER(LEN=*), INTENT(OUT) :: VALUE  ! The value associated with the key

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------

! Check the correct internal array is allocated
IF ( .NOT. ASSOCIATED(dict_val%char_values) )                                  &
  CALL log_fatal("dict_get_char",                                              &
                 "Dictionary is not of character type")

! Try to locate the given key
DO i = 1,dict_val%length
  IF ( dict_val%keys(i) == key ) THEN
    VALUE = dict_val%char_values(i)
    RETURN
  END IF
END DO

! If we get to here, the key does not exist
CALL log_fatal("dict_get_char",                                                &
               "Dictionary does not contain key '" // TRIM(key) // "'")

RETURN

END SUBROUTINE dict_get_char


!-----------------------------------------------------------------------------
! Routines to set keys
!-----------------------------------------------------------------------------
SUBROUTINE dict_set_int(dict_val, key, VALUE)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN OUT) :: dict_val  ! The dictionary to set key on
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to set
INTEGER, INTENT(IN) :: VALUE  ! The value to give the key

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------

! Check the correct internal array is allocated
IF ( .NOT. ASSOCIATED(dict_val%int_values) )                                   &
  CALL log_fatal("dict_set_int",                                               &
                 "Dictionary is not of integer type")

! Try to locate the given key and overwrite the value if it exists
DO i = 1,dict_val%length
  IF ( dict_val%keys(i) == key ) THEN
    dict_val%int_values(i) = VALUE
    RETURN
  END IF
END DO

! If we get to here, we have not found the key, so add a new key if there is room
IF ( dict_val%length >= SIZE(dict_val%keys) )                                  &
  CALL log_fatal("dict_set_int",                                               &
                 "Cannot set key '" // TRIM(key) // "' - dictionary " //       &
                 "already has maximum number of keys")

dict_val%length = dict_val%length + 1
dict_val%keys(dict_val%length) = key
dict_val%int_values(dict_val%length) = VALUE

RETURN

END SUBROUTINE dict_set_int


SUBROUTINE dict_set_real(dict_val, key, VALUE)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN OUT) :: dict_val  ! The dictionary to set key on
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to set
REAL, INTENT(IN) :: VALUE  ! The value to give the key

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------

! Check the correct internal array is allocated
IF ( .NOT. ASSOCIATED(dict_val%real_values) )                                  &
  CALL log_fatal("dict_set_real",                                              &
                 "Dictionary is not of real type")

! Try to locate the given key and overwrite the value if it exists
DO i = 1,dict_val%length
  IF ( dict_val%keys(i) == key ) THEN
    dict_val%real_values(i) = VALUE
    RETURN
  END IF
END DO

! If we get to here, we have not found the key, so add a new key if there is room
IF ( dict_val%length >= SIZE(dict_val%keys) )                                  &
  CALL log_fatal("dict_set_real",                                              &
                 "Cannot set key '" // TRIM(key) // "' - dictionary " //       &
                 "already has maximum number of keys")

dict_val%length = dict_val%length + 1
dict_val%keys(dict_val%length) = key
dict_val%real_values(dict_val%length) = VALUE

RETURN

END SUBROUTINE dict_set_real


SUBROUTINE dict_set_char(dict_val, key, VALUE)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN OUT) :: dict_val  ! The dictionary to set key on
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to set
CHARACTER(LEN=*), INTENT(IN) :: VALUE  ! The value to give the key

! Work variables
INTEGER :: i  ! Loop counter

!-----------------------------------------------------------------------------

! Check the correct internal array is allocated
IF ( .NOT. ASSOCIATED(dict_val%char_values) )                                  &
  CALL log_fatal("dict_set_char",                                              &
                 "Dictionary is not of character type")

! Try to locate the given key and overwrite the value if it exists
DO i = 1,dict_val%length
  IF ( dict_val%keys(i) == key ) THEN
    dict_val%char_values(i) = VALUE
    RETURN
  END IF
END DO

! If we get to here, we have not found the key, so add a new key if there is room
IF ( dict_val%length >= SIZE(dict_val%keys) )                                  &
  CALL log_fatal("dict_set_char",                                              &
                 "Cannot set key '" // TRIM(key) // "' - dictionary " //       &
                 "already has maximum number of keys")

dict_val%length = dict_val%length + 1
dict_val%keys(dict_val%length) = key
dict_val%char_values(dict_val%length) = VALUE

RETURN

END SUBROUTINE dict_set_char


!-----------------------------------------------------------------------------
! Routine to check if a key exists in a dictionary
!-----------------------------------------------------------------------------
FUNCTION dict_has_key(dict_val, key) RESULT(has_key)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN) :: dict_val
CHARACTER(LEN=*), INTENT(IN) :: key  ! The key to check for

! Return type
LOGICAL :: has_key

! Work variables

!-----------------------------------------------------------------------------

! Check if the key exists in the keys array
has_key = ANY(dict_val%keys(1:dict_val%length) == key)

RETURN

END FUNCTION dict_has_key


!-----------------------------------------------------------------------------
! Routine to free resources associated with a dictionary
!-----------------------------------------------------------------------------
SUBROUTINE dict_free(dict_val)

IMPLICIT NONE

! Argument types
TYPE(dict), INTENT(IN OUT) :: dict_val  ! The dictionary to free resources for

!-----------------------------------------------------------------------------

! We just need to make sure all the pointers are nullified
dict_val%length = 0

IF ( ASSOCIATED(dict_val%keys) ) THEN
  DEALLOCATE(dict_val%keys)
  NULLIFY(dict_val%keys)
END IF

IF ( ASSOCIATED(dict_val%int_values) ) THEN
  DEALLOCATE(dict_val%int_values)
  NULLIFY(dict_val%int_values)
END IF

IF ( ASSOCIATED(dict_val%real_values) ) THEN
  DEALLOCATE(dict_val%real_values)
  NULLIFY(dict_val%real_values)
END IF

IF ( ASSOCIATED(dict_val%char_values) ) THEN
  DEALLOCATE(dict_val%char_values)
  NULLIFY(dict_val%char_values)
END IF

RETURN

END SUBROUTINE dict_free

END MODULE dictionary_mod
#endif
