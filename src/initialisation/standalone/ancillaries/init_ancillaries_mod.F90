#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_ancillaries_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_ancillaries(nml_dir, crop_vars, ainfo, trif_vars, urban_param, &
                            trifctltype, rivers, rivers_data)

USE io_constants, ONLY: namelist_unit

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE dump_mod, ONLY: ancil_dump_read

USE init_frac_mod, ONLY: init_frac
USE init_vegetation_props_mod, ONLY: init_vegetation_props
USE init_soil_props_mod, ONLY: init_soil_props
USE init_top_mod, ONLY: init_top
USE init_pdm_mod, ONLY: init_pdm
USE init_agric_mod, ONLY: init_agric
USE init_crop_props_mod, ONLY: init_crop_props
USE init_irrig_props_mod, ONLY: init_irrig_props
USE init_rivers_props_mod, ONLY: init_rivers_props
USE init_water_resources_props_mod, ONLY: init_water_resources_props
USE init_flake_ancils_mod, ONLY: init_flake_ancils
USE init_urban_props_mod, ONLY: init_urban_props
USE init_co2_mod, ONLY: init_co2

USE errormessagelength_mod, ONLY: errormessagelength

USE jules_surface_mod, ONLY: l_urban2t

USE um_types, ONLY: real_jlslsm

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type
USE ancil_info,    ONLY: ainfo_type
USE trif_vars_mod, ONLY: trif_vars_type
USE urban_param_mod, ONLY: urban_param_type
USE trifctl,   ONLY: trifctl_type
USE jules_rivers_mod, ONLY: rivers_type,rivers_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about the model ancillaries
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data

CHARACTER(LEN=errormessagelength) :: iomessage

! Work variables
INTEGER :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: RoutineName='init_ancillaries'

!-----------------------------------------------------------------------------
! Open the ancillaries namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'ancillaries.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file ancillaries.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

! Defer to specialist routines to process each namelist (has to be the same
! order as specified in namelist files)
CALL init_frac()
CALL init_vegetation_props()
CALL init_soil_props()
CALL init_top()
CALL init_pdm()
CALL init_agric(trif_vars%frac_past_gb,trif_vars%frac_biocrop_gb,              &
                trif_vars%harvest_doy,trifctltype)
CALL init_crop_props()
CALL init_irrig_props()
CALL init_rivers_props(rivers, rivers_data)
CALL init_water_resources_props()
IF ( l_urban2t ) CALL init_urban_props(ainfo,urban_param)
CALL init_flake_ancils()
CALL init_co2()

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file ancillaries.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

RETURN

END SUBROUTINE init_ancillaries

END MODULE init_ancillaries_mod

#endif
