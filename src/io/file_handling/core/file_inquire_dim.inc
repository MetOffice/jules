#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_inquire_dim(FILE, dim_name, dim_id, dim_len, is_record_dim)

USE driver_ascii_mod, ONLY: file_ascii_inquire_dim
USE driver_ncdf_mod, ONLY: file_ncdf_inquire_dim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a dimension name, returns its id and its size
!   If the returned id < 0, the dimension doesn't exist
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_handle), INTENT(IN) :: FILE  ! The file to inspect
CHARACTER(LEN=*), INTENT(IN) :: dim_name  ! The name of the dimension to
                                          ! inquire about
INTEGER, INTENT(OUT) :: dim_id  ! The id of the dimension in the file
INTEGER, INTENT(OUT) :: dim_len  ! The length of the dimension in the file
                                 ! This value is unset if the dimension is
                                 ! a record dimension
LOGICAL, INTENT(OUT) :: is_record_dim  ! Indicates if the named dimension
                                       ! is the record dimension for the file


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_inquire_dim(                                                 &
    FILE%ascii, dim_name, dim_id, dim_len, is_record_dim                       &
  )

CASE ( driver_ncdf )
  CALL file_ncdf_inquire_dim(                                                  &
    FILE%ncdf, dim_name, dim_id, dim_len, is_record_dim                        &
  )

CASE DEFAULT
  CALL log_fatal("file_inquire_dim",                                           &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_inquire_dim
#endif
