
#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE data_cube_mod

USE io_constants, ONLY: mdi

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Type definitions
!-----------------------------------------------------------------------------
TYPE :: data_cube
  !-----------------------------------------------------------------------------
  ! This type encapsulates information about a cube of data
  !-----------------------------------------------------------------------------
  INTEGER, POINTER :: SHAPE(:) => NULL()  ! The size of each dimension of
                                          ! the cube
  REAL, POINTER :: values(:) => NULL()  ! The data contained in the cube

END TYPE data_cube

!-----------------------------------------------------------------------------
! Overloads
!-----------------------------------------------------------------------------
! We want to use a single name to convert arrays of different dimensionalities
! to cubes
INTERFACE cube_from_array
  MODULE PROCEDURE cube_from_array_1d,     cube_from_array_2d,                 &
                   cube_from_array_3d,     cube_from_array_4d,                 &
                   cube_from_array_5d,     cube_from_array_6d,                 &
                   cube_from_array_7d
END INTERFACE cube_from_array

! We want to use a single name to read data from the cube in different
! dimensionalities
INTERFACE cube_get_data
  MODULE PROCEDURE cube_get_data_scalar, cube_get_data_1d,                     &
                   cube_get_data_2d,     cube_get_data_3d,                     &
                   cube_get_data_4d,     cube_get_data_5d,                     &
                   cube_get_data_6d,     cube_get_data_7d
END INTERFACE cube_get_data

! Operators
INTERFACE OPERATOR( + )
  MODULE PROCEDURE cube_add_scalar, cube_add_cube
END INTERFACE

INTERFACE OPERATOR( - )
  MODULE PROCEDURE cube_sub_scalar, cube_sub_cube
END INTERFACE

INTERFACE OPERATOR( * )
  MODULE PROCEDURE cube_mul_scalar, cube_mul_cube
END INTERFACE

INTERFACE OPERATOR( / )
  MODULE PROCEDURE cube_div_scalar, cube_div_cube
END INTERFACE


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Public types
    data_cube,                                                                 &
! Public operators
    OPERATOR( + ), OPERATOR( - ), OPERATOR( * ), OPERATOR( / ),                &
! Public procedures
    cube_create, cube_safe_copy, cube_from_array, cube_get_data, cube_free,    &
    cube_min, cube_max, cube_collapse_dimension


CONTAINS


#include "cube_create.inc"
#include "cube_safe_copy.inc"
#include "cube_from_array.inc"
#include "cube_operators.inc"
#include "cube_min_max.inc"
#include "cube_get_data.inc"
#include "cube_free.inc"
#include "cube_collapse_dimension.inc"

END MODULE data_cube_mod
#endif
