#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE cube_free(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Frees the memory used by the given cube. Data will no longer be able to
!   be read
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN OUT) :: cube  ! The cube to free


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Just deallocate the arrays
!-----------------------------------------------------------------------------
IF ( ASSOCIATED(cube%SHAPE) ) THEN
  DEALLOCATE(cube%SHAPE)
  NULLIFY(cube%SHAPE)
END IF

IF ( ASSOCIATED(cube%values) ) THEN
  DEALLOCATE(cube%values)
  NULLIFY(cube%values)
END IF

RETURN

END SUBROUTINE cube_free
#endif
