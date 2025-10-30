#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

FUNCTION cube_from_array_1d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_1d


FUNCTION cube_from_array_2d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:,:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_2d


FUNCTION cube_from_array_3d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:,:,:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_3d


FUNCTION cube_from_array_4d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:,:,:,:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_4d


FUNCTION cube_from_array_5d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:,:,:,:,:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_5d


FUNCTION cube_from_array_6d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:,:,:,:,:,:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_6d


FUNCTION cube_from_array_7d(values) RESULT(cube)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a cube with the same shape and values as the given array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: values(:,:,:,:,:,:,:)  ! The array to create the cube from


! Return type
TYPE(data_cube) :: cube  ! The cube containing the data


!-----------------------------------------------------------------------------


cube = cube_create(SHAPE(values))
cube%values(:) = RESHAPE(values, [ SIZE(values) ])

RETURN

END FUNCTION cube_from_array_7d
#endif
