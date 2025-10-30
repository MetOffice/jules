#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE get_var_attrs(var_id, int_attributes, real_attributes,              &
                         char_attributes)

USE io_constants, ONLY: max_attr_var, mdi

USE dictionary_mod, ONLY: dict, dict_create, dict_set

USE string_utils_mod, ONLY: to_string

USE jules_soil_biogeochem_mod, ONLY:                                           &
!  imported scalar parameters
     soil_model_ecosse, soil_model_4pool, soil_model_1pool,                    &
!  imported scalars (IN)
     soil_bgc_model

USE jules_soil_mod, ONLY: zsmc

USE jules_vegetation_mod, ONLY: n_day_photo_acclim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given an identifer for a model variable, returns the attributes that
!   should be defined on output files
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: var_id  ! Identifies the model variable
TYPE(dict), INTENT(OUT) :: int_attributes   ! Attributes with integer values
TYPE(dict), INTENT(OUT) :: real_attributes  ! Attributes with real values
TYPE(dict), INTENT(OUT) :: char_attributes  ! Attributes with character
                                            ! values


! Work variables
CHARACTER(LEN=identifier_len) :: identifier  ! The string identifier for the
                                             ! variable


!-----------------------------------------------------------------------------


! Create the dictionary objects
int_attributes = dict_create(max_attr_var, INT(1))
real_attributes = dict_create(max_attr_var, REAL(1.0))
char_attributes = dict_create(max_attr_var, "char")


! Get the string identifier
identifier = get_string_identifier(var_id)


! All variables except lat and lon will have a missing data attribute and
! a coordinate attribute.
! lat and lon do not need these since they are interpreted as coordinate
! variables as per the CF convention.
IF ( identifier /= 'latitude' .AND. identifier /= 'longitude' ) THEN
  CALL dict_set(real_attributes, "missing_value", mdi)
  CALL dict_set(real_attributes, "_FillValue", mdi)
  CALL dict_set(char_attributes, "coordinates", "latitude longitude")
END IF


! Add the standard "long_name" and "units" attributes from the metadata array
! var_id is an index into the metadata array
CALL dict_set(char_attributes, "long_name", metadata(var_id)%long_name)
CALL dict_set(char_attributes, "units", metadata(var_id)%units)


! Use a select statement to add extra attributes/override current values if
! required. The SELECT statement uses the string identifier to select a CASE
! to avoid being dependent on the implementation of integer ids.
SELECT CASE ( identifier )
CASE ( 'latitude' )
  CALL dict_set(char_attributes, "standard_name", "latitude")

CASE ( 'longitude' )
  CALL dict_set(char_attributes, "standard_name", "longitude")

CASE ( 'frac' )
  CALL dict_set(real_attributes, "valid_min", 0.0)
  CALL dict_set(real_attributes, "valid_max", 1.0)

CASE ( 'smc_avail_top' )
  ! Overwrite long_name to use a value that is calculated using zsmc.
  CALL dict_set(char_attributes, "long_name",                                  &
                "Gridbox available moisture in top " //                        &
                TRIM(to_string(zsmc)) // "m of soil")

CASE ( 'cs' )
  ! Overwrite long_name to use a value that specifies the soil C pools.
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_ecosse, soil_model_4pool )
    CALL dict_set(char_attributes, "long_name",                                &
                  "Gridbox soil carbon in each pool (DPM,RPM,bio,hum)")
  CASE ( soil_model_1pool)
    CALL dict_set(char_attributes, "long_name",                                &
                  "Gridbox soil carbon (single pool)")
  END SELECT

CASE ( 'resp_s' )
  ! Overwrite long_name to use a value that specifies the soil C pools.
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_ecosse, soil_model_4pool )
    CALL dict_set(char_attributes, "long_name",                                &
      "Gridbox soil respiration from each pool (DPM,RPM,bio,hum)")
  CASE ( soil_model_1pool)
    CALL dict_set(char_attributes, "long_name",                                &
      "Gridbox soil respiration (single pool)")
  END SELECT

CASE ( 'temp_ave_nday' )
  ! Overwrite long_name to include length of average.
  CALL dict_set(char_attributes, "long_name",                                  &
                "Average temperature (exp filter) over previous " //           &
                TRIM(to_string(n_day_photo_acclim)) // " days")

END SELECT

RETURN

END SUBROUTINE get_var_attrs
#endif
