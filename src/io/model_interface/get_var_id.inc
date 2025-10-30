#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION get_var_id(identifier) RESULT(var_id)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a string identifier for a model variable, returns the integer id for
!   that variable
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: identifier  ! Identifies the model variable

! Return type
INTEGER :: var_id  ! The integer id for the variable


! Work variables
INTEGER :: i  ! Loop index


!-----------------------------------------------------------------------------


! The returned variable id is the index in the metadata array, so we just
! have to search for the given identifier in the metadata entries
var_id = -1

DO i = 1,n_vars
  IF ( metadata(i)%identifier == identifier ) THEN
    var_id = i
    RETURN
  END IF
END DO

! If we failed to find the identifier, error out
IF ( var_id < 1 )                                                              &
  CALL log_fatal("get_var_id",                                                 &
                 "Unrecognised variable identifier - '" //                     &
                 TRIM(identifier) // "'. See docs for available variables")

RETURN

END FUNCTION get_var_id
#endif
