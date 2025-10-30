#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT**************************************

FUNCTION is_river_var(identifier) RESULT(l_river_var)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a string identifier for a model variable, returns TRUE if the
!   variable is on river points (var_type_rp), otherwise FALSE.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: identifier  ! Identifies the model variable

! Return type
LOGICAL :: l_river_var
  ! TRUE if variable is of type var_type_rp, otherwise FALSE

! Work variables
INTEGER :: var_id   ! The integer id for the variable.
INTEGER :: var_type ! Code giving the type of the variable.

!-----------------------------------------------------------------------------

! Initialise result.
l_river_var = .FALSE.

! Get the variable's ID.
var_id = get_var_id( identifier )

! Get the type of variable.
var_type = metadata(var_id) % var_type

IF ( var_type == var_type_rp ) l_river_var = .TRUE.

RETURN

END FUNCTION is_river_var
#endif
