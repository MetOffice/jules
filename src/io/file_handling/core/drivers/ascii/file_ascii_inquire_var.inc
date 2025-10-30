#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_inquire_var(FILE, var_name, var_id, ndims, dim_ids, is_record)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a variable name, returns its id and information about dimensions
!   If the returned id < 0, the varension doesn't exist
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
CHARACTER(LEN=*), INTENT(IN) :: var_name  ! The name of the variable to
                                          ! inquire about
INTEGER, INTENT(OUT) :: var_id  ! The id of the variable in the file
INTEGER, INTENT(OUT) :: ndims  ! The number of dimensions that
                                         ! the variable has
INTEGER, INTENT(OUT) :: dim_ids(:)  ! The dimension ids
LOGICAL, INTENT(OUT) :: is_record   ! Indicates if the variable
                                    ! uses the record dimension

! Work variables
INTEGER :: i  ! Loop counter


!-----------------------------------------------------------------------------


! If we are still in define mode, we can't inspect
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ascii_inquire_var",                                     &
                 "Cannot inquire file in define mode")

! Find the index in the vars arrays of the variable with the given name
! This will be the returned variable id
var_id = -1
DO i = 1,FILE%nvars
  IF ( FILE%var_names(i) == var_name ) THEN
    var_id = i
    EXIT
  END IF
END DO

! If we didn't find the variable name, we are done
IF ( var_id < 1 ) RETURN

! Otherwise, the var_id is the location in the array, and we can easily get
! the number of dimensions and their ids
ndims = FILE%var_ndims(var_id)
dim_ids(1:ndims) = FILE%var_dims(var_id,1:ndims)

! Indicate if the variable uses the record dimension
! In ASCII files, either all variables do or none do
is_record = FILE%has_record_dim

RETURN

END SUBROUTINE file_ascii_inquire_var
#endif
