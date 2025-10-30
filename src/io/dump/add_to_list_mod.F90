! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE add_to_list_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE add_to_list( identifier, nvars, varlist, l_append )

! Adds an identifier to a list, optionally appending a suffix.

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with intent(in)
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) ::                                                &
  identifier
    ! An identifier for a variable.

!-----------------------------------------------------------------------------
! Arguments with intent(inout)
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN OUT) ::                                                     &
  nvars
    ! The number of values set in the list.

CHARACTER(LEN=*), INTENT(IN OUT) ::                                            &
  varlist(:)
    ! A list of identifiers.

!-----------------------------------------------------------------------------
! Optional arguments with intent(in)
!-----------------------------------------------------------------------------
LOGICAL, INTENT(IN), OPTIONAL ::                                               &
  l_append
    ! Flag indicating if a suffix should be attached to the identifier.

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'ADD_TO_LIST'
    ! The name of this routine.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_append_soilt_local
    ! A local version of the optional argument l_append.

! end of header
!-----------------------------------------------------------------------------

! Set a local variables depending on the optional argument.
IF ( PRESENT(l_append) ) THEN
  l_append_soilt_local = l_append
ELSE
  l_append_soilt_local = .FALSE.
END IF

!-----------------------------------------------------------------------------
! Append to the list.
!-----------------------------------------------------------------------------
IF ( nvars + 1 > SIZE( varlist ) ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Too many values. Increase size of variable." )
  STOP
ELSE
  nvars          = nvars + 1
  varlist(nvars) = identifier
  IF ( l_append_soilt_local ) THEN
    varlist(nvars) = TRIM(varlist(nvars)) // '_soilt'
  END IF
END IF

RETURN
END SUBROUTINE add_to_list
END MODULE add_to_list_mod
