#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE close_all()

USE data_cube_mod, ONLY: cube_free

USE file_ts_mod, ONLY: file_ts_close

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Closes all the output files and frees all resources consumed by them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER :: i,j  ! Loop counters


!-----------------------------------------------------------------------------

DO i = 1,nprofiles
  ! Free all the data cubes for the output_fields
  DO j = 1,profiles(i)%nfields
    CALL cube_free(profiles(i)%fields(j)%field_data)
  END DO

  DEALLOCATE(profiles(i)%fields)
  NULLIFY(profiles(i)%fields)

  ! Close the file
  ! For river files this is done by the master task, for all other files it is
  ! done by all tasks
  IF ( profiles(i)%has_open_file ) CALL file_ts_close(profiles(i)%fh)
END DO


END SUBROUTINE close_all
#endif
