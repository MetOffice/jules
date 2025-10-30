#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE spinup_mod

USE model_interface_mod, ONLY: identifier_len

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: max_spinup_vars = 4

!-----------------------------------------------------------------------------
! Module types
!-----------------------------------------------------------------------------
TYPE :: spinup_field

  CHARACTER(LEN=identifier_len) :: identifier
                 ! The identifier of the model variable associated with this
                 ! spinup field

  LOGICAL :: use_percent = .FALSE.  ! T - tolerance is specified in %
                                    ! F - tolerance is specified as an
                                    !     absolute value
  REAL :: tolerance = 0.0  ! The tolerance to use for this field

  REAL, POINTER :: DATA(:,:,:) => NULL()  ! The data from the last comparison

END TYPE spinup_field

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
INTEGER :: nvars = 0
TYPE(spinup_field), SAVE :: spinup_vars(max_spinup_vars)

END MODULE spinup_mod
#endif
