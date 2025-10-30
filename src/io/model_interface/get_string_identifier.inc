#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION get_string_identifier(var_id) RESULT(identifier)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given an integer id for a model variable, returns the string identifier
!   for that variable
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: var_id  ! The integer id for the variable

! Return type
CHARACTER(LEN=identifier_len) :: identifier  ! The string identifier for
                                             ! the model variable


!-----------------------------------------------------------------------------


! var_id is an index in the metadata array
identifier = metadata(var_id)%identifier

RETURN

END FUNCTION get_string_identifier
#endif
