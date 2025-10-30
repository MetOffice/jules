#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_inquire_dim(FILE, dim_name, dim_id, dim_len, is_record_dim)

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
TYPE(file_ascii), INTENT(IN) :: FILE  ! The file to inspect
CHARACTER(LEN=*), INTENT(IN) :: dim_name  ! The name of the dimension to
                                          ! inquire about
INTEGER, INTENT(OUT) :: dim_id  ! The id of the dimension in the file
INTEGER, INTENT(OUT) :: dim_len  ! The length of the dimension in the file
LOGICAL, INTENT(OUT) :: is_record_dim  ! Indicates if the named dimension
                                       ! is the record dimension for the file

! Work variables
INTEGER :: i  ! Loop counter


!-----------------------------------------------------------------------------


! If we are still in define mode, we can't inspect
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ascii_inquire_dim",                                     &
                 "Cannot inquire file in define mode")

! Deal with the case that the record dimension has the specified name first
is_record_dim = FILE%has_record_dim .AND. ( FILE%record_dim_name == dim_name )
IF ( is_record_dim ) THEN
  RETURN
END IF

! Find the index in the dims arrays of the dimension with the given name
! This will be the returned dimension id
dim_id = -1
DO i = 1,FILE%ndims
  IF ( FILE%dim_names(i) == dim_name ) THEN
    dim_id = i
    EXIT
  END IF
END DO

! If we didn't find the dimension name, we have nothing more to do
IF ( dim_id < 1 ) RETURN

! Now we can easily get the size
dim_len = FILE%dim_sizes(dim_id)

RETURN

END SUBROUTINE file_ascii_inquire_dim
#endif
