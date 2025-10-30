#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ts_read_var(FILE, var_id, extract_subgrid, subgrid) RESULT(cube)

USE grid_utils_mod, ONLY: subgrid_info

USE file_gridded_mod, ONLY: file_gridded_read_var

USE data_cube_mod, ONLY: data_cube

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads data from the given variable in the given file as a data cube
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ts), INTENT(IN OUT) :: FILE
                               ! The file to read data from
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
LOGICAL, INTENT(IN) :: extract_subgrid
                               ! T - extract a subgrid to return from the
                               !     full grid of the file
                               ! F - return the full grid
TYPE(subgrid_info), OPTIONAL :: subgrid  ! The subgrid to extract


! Return type
TYPE(data_cube) :: cube  ! The data cube read from file


! Work variables
INTEGER :: actual_var_id  ! The id of the variable in the underlying file


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Defer the actual reading to the file_gridded routine
!-----------------------------------------------------------------------------
actual_var_id = FILE%vars(var_id)%current_id

IF ( PRESENT(subgrid) ) THEN
  cube = file_gridded_read_var(                                                &
    FILE%open_file, actual_var_id, extract_subgrid, subgrid                    &
  )
ELSE
  cube = file_gridded_read_var(                                                &
    FILE%open_file, actual_var_id, extract_subgrid                             &
  )
END IF

RETURN

END FUNCTION file_ts_read_var
#endif
