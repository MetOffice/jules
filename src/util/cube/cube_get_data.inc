#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE cube_get_data_scalar(cube, VALUE)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a scalar
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: VALUE  ! The scalar value to retrieve


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the data in the cube is of the correct size to extract a scalar
!-----------------------------------------------------------------------------
IF ( SIZE(cube%values) /= 1 )                                                  &
  CALL log_fatal("cube_get_data_scalar",                                       &
                 "Cube must have size 1 to extract a scalar")

!-----------------------------------------------------------------------------
! Extract the scalar and return it
!-----------------------------------------------------------------------------
VALUE = cube%values(1)

RETURN

END SUBROUTINE cube_get_data_scalar


SUBROUTINE cube_get_data_1d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 1d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:)  ! The array to put values in


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 1 )                                                   &
  CALL log_fatal("cube_get_data_1d",                                           &
                 "values has a different rank to the cube")

! Check that values is the correct size
IF ( SIZE(values) /= cube%SHAPE(1) )                                           &
  CALL log_fatal("cube_get_data_1d",                                           &
                 "values is not the same size as the cube")

!-----------------------------------------------------------------------------
! The cube's values are already stored as a 1d array
!-----------------------------------------------------------------------------
values(:) = cube%values(:)

RETURN

END SUBROUTINE cube_get_data_1d


SUBROUTINE cube_get_data_2d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 2d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:,:)  ! The array to put values in

INTEGER :: shape2D(2) ! Constant size array required by RESHAPE intrinsic

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 2 )                                                   &
  CALL log_fatal("cube_get_data_2d",                                           &
                 "values has a different rank to the cube")

! Check that all the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:2)) )                             &
  CALL log_fatal("cube_get_data_2d",                                           &
                 "values is not the same shape as the cube")

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape2D(1:2) = cube%SHAPE(1:2)
values = RESHAPE(cube%values, shape2D)

RETURN

END SUBROUTINE cube_get_data_2d


SUBROUTINE cube_get_data_3d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 3d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:,:,:)  ! The array to put values in

INTEGER :: shape3D(3) ! Constant size array required by RESHAPE intrinsic

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 3 )                                                   &
  CALL log_fatal("cube_get_data_3d",                                           &
                 "values has a different rank to the cube")

! Check that all the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:3)) )                             &
  CALL log_fatal("cube_get_data_3d",                                           &
                 "values is not the same shape as the cube")

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape3D(1:3) = cube%SHAPE(1:3)
values = RESHAPE(cube%values, shape3D)

RETURN

END SUBROUTINE cube_get_data_3d


SUBROUTINE cube_get_data_4d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 4d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:,:,:,:)  ! The array to put values in

INTEGER :: shape4D(4) ! Constant size array required by RESHAPE intrinsic

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 4 )                                                   &
  CALL log_fatal("cube_get_data_4d",                                           &
                 "values has a different rank to the cube")

! Check that all the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:4)) )                             &
  CALL log_fatal("cube_get_data_4d",                                           &
                 "values is not the same shape as the cube")

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape4D(1:4) = cube%SHAPE(1:4)
values = RESHAPE(cube%values, shape4D)

RETURN

END SUBROUTINE cube_get_data_4d


SUBROUTINE cube_get_data_5d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 5d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:,:,:,:,:)  ! The array to put values in

INTEGER :: shape5D(5) ! Constant size array required by RESHAPE intrinsic

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 5 )                                                   &
  CALL log_fatal("cube_get_data_5d",                                           &
                 "values has a different rank to the cube")

! Check that all the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:5)) )                             &
  CALL log_fatal("cube_get_data_5d",                                           &
                 "values is not the same shape as the cube")

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape5D(1:5) = cube%SHAPE(1:5)
values = RESHAPE(cube%values, shape5D)

RETURN

END SUBROUTINE cube_get_data_5d


SUBROUTINE cube_get_data_6d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 6d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:,:,:,:,:,:)  ! The array to put values in

INTEGER :: shape6D(6) ! Constant size array required by RESHAPE intrinsic

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 6 )                                                   &
  CALL log_fatal("cube_get_data_6d",                                           &
                 "values has a different rank to the cube")

! Check that all the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:6)) )                             &
  CALL log_fatal("cube_get_data_6d",                                           &
                 "values is not the same shape as the cube")

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape6D(1:6) = cube%SHAPE(1:6)
values = RESHAPE(cube%values, shape6D)

RETURN

END SUBROUTINE cube_get_data_6d


SUBROUTINE cube_get_data_7d(cube, values)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Get the data from a cube where the data is a 7d array
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube  ! The cube containing the data

REAL, INTENT(OUT) :: values(:,:,:,:,:,:,:)  ! The array to put values in

INTEGER :: shape7D(7) ! Constant size array required by RESHAPE intrinsic

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 7 )                                                   &
  CALL log_fatal("cube_get_data_7d",                                           &
                 "values has a different rank to the cube")

! Check that all the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:7)) )                             &
  CALL log_fatal("cube_get_data_7d",                                           &
                 "values is not the same shape as the cube")

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape7D(1:7) = cube%SHAPE(1:7)
values = RESHAPE(cube%values, shape7D)

RETURN

END SUBROUTINE cube_get_data_7d
#endif
