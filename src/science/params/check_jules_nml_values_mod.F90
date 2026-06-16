! ***************************COPYRIGHT*****************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! ***************************COPYRIGHT*****************************
MODULE check_jules_nml_values_mod

IMPLICIT NONE

! Description:
!   Routine for checking JULES namelist inputs
!
! Method:
!   Checks the namelist value is in the correct range taking account of
!   special cases.
!
! Code Owner: Please refer to the JULES file CodeOwners.txt
!
! Code description:
!  Language: Fortran 2003.
!  This code is written to UMDP3 standards.
!
INTERFACE check_jules_nml_values
  MODULE PROCEDURE check_jules_nml_values_real
END INTERFACE check_jules_nml_values

CONTAINS

SUBROUTINE check_jules_nml_values_real ( var, var_name, var_size,  min_value,  &
   max_value, RoutineName, errorstatus )

USE jules_print_mgr, ONLY: jules_print, jules_message
USE jules_surface_types_mod, ONLY: soil, npft
USE missing_data_mod, ONLY: rmdi
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER                :: var_size, errorstatus
REAL(KIND=real_jlslsm) :: var(var_size), min_value, max_value
CHARACTER(LEN=*)       :: var_name, RoutineName
LOGICAL                :: soil_chck(var_size)

!-----------------------------------------------------------------------------
! The namelist variables should be initialised to rmdi and will still be
! rmdi if they are attached to science options when not required (UM/JULES).
! Specific checks to ensure that these required variables are not rmdi can be
! found in check_compatible_options.
!-----------------------------------------------------------------------------

IF ( ANY( ABS( var(:) - rmdi ) > EPSILON(1.0) ) ) THEN
  IF ( ANY( var(:) > max_value ) ) errorstatus = 2
  IF ( ANY( var(:) < min_value ) ) THEN
    ! Need to account for albsnf_nvg(soil) = -1 for ancil
    IF ( var_name == 'albsnf_nvg' ) THEN
      soil_chck(:) = ( var(:) < min_value )
      SELECT CASE ( COUNT( soil_chck(:) ) )
      CASE ( 1 )
        IF ( .NOT. ABS ( var(soil-npft) + 1.0 ) < EPSILON(1.0) ) THEN
          errorstatus = 3
        END IF
      CASE DEFAULT
        errorstatus = 3
      END SELECT
    ELSE
      errorstatus = 3
    END IF
  END IF
  IF ( errorstatus > 1 ) THEN
    WRITE(jules_message,*) TRIM(var_name) // ' is out of range: ', var(:)
    CALL jules_print(RoutineName, jules_message)
    ! Reset to fail value after printing message
    errorstatus = 1
  END IF
END IF

END SUBROUTINE check_jules_nml_values_real

END MODULE check_jules_nml_values_mod
