#if !defined(UM_JULES)

SUBROUTINE check_variable_metadata

! Check that the metadata for input/output variables do not include
! duplicate names.

IMPLICIT NONE

! Local scalar parameters.
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'CHECK_VARIABLE_METADATA'

! Local scalar variables.
INTEGER :: i  !  Loop counter.

!-----------------------------------------------------------------------------
! It is possible that duplicate names have been coded for variables.
! Duplicates that are coded for input or output will be detected during
! compilation because the SELECT statements in populate_var and extract_var
! will not work. But it is possible that one variable can be included in the
! input code and the other in the output code (or, unusually, included in
! neither!) in which case the duplication will not be detected, with
! unexpected results during run time.
! Here we guard against that possibility by checking the metadata for
! duplicates.
! It is possible that this code will prevent an acceptable run (if that run
! does not need to access a duplicate variable), but in general it is better
! to test and prevent duplicate names.
!-----------------------------------------------------------------------------
DO i = 1, n_vars - 1

  ! Check if any later variable has the same name.
  IF ( ANY( metadata(i+1:n_vars)%identifier == metadata(i)%identifier ) )      &
    THEN

    CALL log_fatal(RoutineName,                                                &
                   "Duplicate name=" // TRIM(metadata(i)%identifier ) //       &
                   ". Code needs to be changed!")
  END IF

END DO

RETURN

END SUBROUTINE check_variable_metadata
#endif
