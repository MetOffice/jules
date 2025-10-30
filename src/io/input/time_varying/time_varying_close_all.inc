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
!   Closes all the time varying input files and frees all resources
!   consumed by them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER :: i, j, k  ! Loop counters


!-----------------------------------------------------------------------------

DO i = 1,nfiles
  DO j = 1,files(i)%nfields
    ! Deallocate all the data cubes for the input field
    DO k = files(i)%times_lbound,files(i)%times_ubound
      CALL cube_free(files(i)%fields(j)%DATA(k))
    END DO

    ! Deallocate the cube store for the field
    DEALLOCATE(files(i)%fields(j)%DATA)
    NULLIFY(files(i)%fields(j)%DATA)
  END DO

  ! Deallocate the fields array for the file
  DEALLOCATE(files(i)%fields)
  NULLIFY(files(i)%fields)

  ! Close the underlying file
  CALL file_ts_close(files(i)%fh)
END DO

RETURN

END SUBROUTINE close_all
#endif
