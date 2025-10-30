#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_grid_mod

CONTAINS

SUBROUTINE init_grid(nml_dir, psparms_data, ainfo_data, progs_data,            &
                     coastal_data,                                             &
                     fluxes_data,                                              &
                     rivers_data,                                              &
                     wtrac_jls_data)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE errormessagelength_mod, ONLY: errormessagelength

USE logging_mod, ONLY: log_fatal

USE init_input_grid_mod,        ONLY: init_input_grid
USE init_latlon_mod,            ONLY: init_latlon
USE init_land_frac_mod,         ONLY: init_land_frac
USE init_model_grid_mod,        ONLY: init_model_grid
USE init_model_grid_arrays_mod, ONLY: init_model_grid_arrays

!TYPE definitions
USE p_s_parms,            ONLY: psparms_data_type
USE ancil_info,           ONLY: ainfo_data_type
USE prognostics,          ONLY: progs_data_type
USE coastal,              ONLY: coastal_data_type
USE fluxes_mod,           ONLY: fluxes_data_type
USE jules_rivers_mod,     ONLY: rivers_data_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about the model grids
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
TYPE(psparms_data_type),   INTENT(IN OUT) :: psparms_data
TYPE(ainfo_data_type),     INTENT(IN OUT) :: ainfo_data
TYPE(progs_data_type),     INTENT(IN OUT) :: progs_data
TYPE(coastal_data_type),   INTENT(IN OUT) :: coastal_data
TYPE(fluxes_data_type),    INTENT(IN OUT) :: fluxes_data
TYPE(rivers_data_type),    INTENT(IN OUT) :: rivers_data
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------


! Open the grid namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'model_grid.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_grid",                                                  &
                 "Error opening namelist file model_grid.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Defer to specialised routines to initialise the different aspect of the grid
CALL init_input_grid()
CALL init_latlon()
CALL init_land_frac()
CALL init_model_grid()
!-----------------------------------------------------------------------------
! Allocate Standalone Rivers arrays and fill model grid arrays
!-----------------------------------------------------------------------------
CALL init_model_grid_arrays( psparms_data, ainfo_data,                         &
                           progs_data, coastal_data,                           &
                           fluxes_data, rivers_data, wtrac_jls_data            &
                           )

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_grid",                                                  &
                 "Error closing namelist file model_grid.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

RETURN

END SUBROUTINE init_grid

END MODULE init_grid_mod
#endif
