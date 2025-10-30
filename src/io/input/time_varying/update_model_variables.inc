#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE update_model_variables()

USE data_cube_mod, ONLY: data_cube, cube_free

USE interpolation_mod, ONLY: interpolate

USE model_interface_mod, ONLY: populate_var

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Updates model variables with data for the current model time
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
TYPE(data_cube) :: interp_data  ! Cube for the interpolated data that can be freed

INTEGER :: i,j  ! Loop counter


!-----------------------------------------------------------------------------

DO i = 1,nfiles
  DO j = 1,files(i)%nfields
    ! We want to fill the model variable with interpolated data
    interp_data = interpolate(                                                 &
      files(i)%fields(j)%DATA,                                                 &
      files(i)%fields(j)%interp_flag,                                          &
      files(i)%tsteps_in_data_period,                                          &
      files(i)%current_tstep                                                   &
    )

    CALL populate_var(files(i)%fields(j)%var_id, interp_data)

    CALL cube_free(interp_data)
  END DO  ! fields
END DO  ! files

RETURN

END SUBROUTINE update_model_variables
#endif
