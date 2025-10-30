#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE internal_define_var(FILE, defined_dims, var_id, var_name,           &
                               use_time, var_file_id)

USE io_constants, ONLY: max_dim_var

USE dictionary_mod, ONLY: dict_key_len, dict, dict_char_val_len,               &
                           dict_has_key, dict_get, dict_set, dict_free

USE model_interface_mod, ONLY: get_var_levs_dims, get_var_attrs


USE file_ts_mod, ONLY: file_ts_def_dim, file_ts_def_var, file_ts_def_attr

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   INTERNAL PROCEDURE TO output_mod
!   Defines the variable identified by var_id on the given file_ts object
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ts), INTENT(IN OUT) :: FILE  ! The file to define variables on
TYPE(dict), INTENT(IN OUT) :: defined_dims
                                      ! Mapping of dim_name => dim_id for
                                      ! dimensions already defined on the
                                      ! file
INTEGER, INTENT(IN) :: var_id
                              ! The variable id of the model variable to
                              ! define on the file
                              ! See model_interface_mod
CHARACTER(LEN=*), INTENT(IN) :: var_name
                              ! The name to use for the variable in
                              ! output files
LOGICAL, INTENT(IN) :: use_time
                              !   T - create the variable as using time
                              !       dimension
                              !   F - create the variable as not using time
                              !       dimension
INTEGER, INTENT(OUT) :: var_file_id
                             ! The id of the variable in the file


! Work variables
INTEGER :: var_ndims   ! The number of levels dimensions the variable has
INTEGER :: lev_dim_sizes(max_dim_var)  ! The sizes of the levels dimensions
CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
                                    ! The names of the levels dimensions for
                                    ! the variable
INTEGER :: dim_ids(max_dim_var)    ! The ids in file of the levels dimensions

TYPE(dict) :: int_attrs, real_attrs, char_attrs  ! Dictionaries containing
                                                 ! attribute values

CHARACTER(LEN=dict_key_len) :: key  ! Used when iterating over attribute
INTEGER :: int_val                  ! dictionaries
REAL :: real_val
CHARACTER(LEN=dict_char_val_len) :: char_val

INTEGER :: i  ! Index variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Get the ids of the levels dimensions for the variable, defining them if
! they have not been defined
!-----------------------------------------------------------------------------
! is_climatology will only be true for initial conditions initialised from a
! climatology ancillary. It therefore must be FALSE in the following call.
CALL get_var_levs_dims(var_id, is_climatology = .FALSE., ndims = var_ndims,    &
                       dim_names_out = dim_names, dim_sizes = lev_dim_sizes)

DO i = 1,var_ndims
  ! If it has not yet been defined, define the dimension, storing its id
  IF ( .NOT. dict_has_key(defined_dims, dim_names(i)) )                        &
    CALL dict_set(                                                             &
      defined_dims, dim_names(i),                                              &
      file_ts_def_dim(FILE, dim_names(i), lev_dim_sizes(i))                    &
    )

  ! Get the dimension id from the dict and add it to the list for this variable
  CALL dict_get(defined_dims, dim_names(i), dim_ids(i))
END DO

!-----------------------------------------------------------------------------
! Create the variable and store its id
!-----------------------------------------------------------------------------
var_file_id = file_ts_def_var(                                                 &
  FILE, var_name, dim_ids(1:var_ndims), use_time                               &
)

!-----------------------------------------------------------------------------
! Define attributes
!-----------------------------------------------------------------------------
CALL get_var_attrs(var_id, int_attrs, real_attrs, char_attrs)

! First the integer valued attributes
DO i = 1,int_attrs%length
  key = int_attrs%keys(i)
  CALL dict_get(int_attrs, key, int_val)

  CALL file_ts_def_attr(FILE, var_file_id, key, int_val)
END DO

! Next, real valued attributes
DO i = 1,real_attrs%length
  key = real_attrs%keys(i)
  CALL dict_get(real_attrs, key, real_val)

  CALL file_ts_def_attr(FILE, var_file_id, key, real_val)
END DO

! Lastly, character valued attributes
DO i = 1,char_attrs%length
  key = char_attrs%keys(i)
  CALL dict_get(char_attrs, key, char_val)

  CALL file_ts_def_attr(FILE, var_file_id, key, char_val)
END DO

! Free the attribute dictionaries
CALL dict_free(int_attrs)
CALL dict_free(real_attrs)
CALL dict_free(char_attrs)

RETURN

END SUBROUTINE internal_define_var
#endif
