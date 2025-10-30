! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_mod
CONTAINS
SUBROUTINE init(nml_dir,                                                       &
                psparms_data, psparms,                                         &
                ainfo, ainfo_data,                                             &
                progs, progs_data,                                             &
                coastal_data, coast,                                           &
                fluxes_data, fluxes,                                           &
                rivers_data, rivers,                                           &
                wtrac_jls_data, wtrac_jls)

USE model_interface_mod,            ONLY: check_variable_metadata
USE time_varying_input_mod,         ONLY: seek_all_to_current_datetime
USE init_grid_mod,                  ONLY: init_grid
USE init_ic_mod,                    ONLY: init_ic
USE init_ancillaries_mod,           ONLY: init_ancillaries
USE write_dump_mod,                 ONLY: write_dump
USE logging_mod,                    ONLY: log_info, init_prnt_control
USE init_output_mod,                ONLY: init_output
USE init_model_environment_mod,     ONLY: init_model_environment
USE init_rivers_mod,                ONLY: init_rivers
USE jules_science_fixes_mod,        ONLY: init_science_fixes
USE init_time_mod,                  ONLY: init_time
USE init_oasis_rivers_mod,          ONLY: init_oasis_rivers
USE oasis_rivers_control_mod,       ONLY: oasis_partition, oasis_grid,         &
                                          oasis_variables
USE jules_model_environment_mod,    ONLY: l_oasis_rivers
USE init_drive_mod,                 ONLY: init_drive
USE check_compatible_options_mod,   ONLY: check_compatible_options

USE jules_print_mgr, ONLY:                                                     &
    jules_message,                                                             &
    jules_print

!TYPE definitions
USE p_s_parms,                    ONLY: psparms_data_type,                     &
                                        psparms_type,                          &
                                        psparms_assoc
USE ancil_info,                   ONLY: ainfo_data_type,                       &
                                        ainfo_type,                            &
                                        ancil_info_assoc
USE prognostics,                  ONLY: progs_data_type,                       &
                                        progs_type,                            &
                                        prognostics_assoc
USE coastal,                      ONLY: coastal_data_type,                     &
                                        coastal_type,                          &
                                        coastal_assoc
USE fluxes_mod,                   ONLY: fluxes_data_type,                      &
                                        fluxes_type,                           &
                                        fluxes_assoc
USE jules_rivers_mod,             ONLY: rivers_data_type,                      &
                                        rivers_type,                           &
                                        rivers_assoc
USE jules_wtrac_type_mod,         ONLY: jls_wtrac_data_type,                   &
                                        jls_wtrac_type,                        &
                                        wtrac_jls_assoc

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   The main Rivers-standalone initialisation routine - initialises the model
!   by calling specific routines (based on JULES init.F90)
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

!TYPES containing the data passed down to be allocated when possible
TYPE(psparms_data_type),   INTENT(IN OUT) :: psparms_data
TYPE(ainfo_data_type),     INTENT(IN OUT) :: ainfo_data
TYPE(progs_data_type),     INTENT(IN OUT) :: progs_data
TYPE(coastal_data_type),   INTENT(IN OUT) :: coastal_data
TYPE(fluxes_data_type),    INTENT(IN OUT) :: fluxes_data
TYPE(rivers_data_type),    INTENT(IN OUT) :: rivers_data
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

!TYPES pointing to data
TYPE(psparms_type),   INTENT(IN OUT) :: psparms
TYPE(ainfo_type),     INTENT(IN OUT) :: ainfo
TYPE(progs_type),     INTENT(IN OUT) :: progs
TYPE(coastal_type),   INTENT(IN OUT) :: coast
TYPE(fluxes_type),    INTENT(IN OUT) :: fluxes
TYPE(rivers_type),    INTENT(IN OUT) :: rivers
TYPE(jls_wtrac_type), INTENT(IN OUT) :: wtrac_jls

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT'

!-----------------------------------------------------------------------------

! Check that the metadata for variables is acceptable.
CALL check_variable_metadata

! Set options for output of diagnostic and informative messages.
CALL init_prnt_control(nml_dir)

! Determine what environment JULES is being run in
CALL init_model_environment(nml_dir)

! Intialise the times for the run
CALL init_time(nml_dir)

! Initialise river routing parameters, ancils and grid
CALL init_rivers(nml_dir)

IF (l_oasis_rivers) THEN
  ! Initialize coupling parameters
  CALL init_oasis_rivers(nml_dir)
END IF

! Initialise science fixes
CALL init_science_fixes(nml_dir)

! Initialise the input, model and output grids **also allocates arrays**
CALL init_grid(nml_dir, psparms_data, ainfo_data, progs_data,                  &
               coastal_data,                                                   &
               fluxes_data,                                                    &
               rivers_data,                                                    &
               wtrac_jls_data)

!Associate the data and pointer types
CALL psparms_assoc(psparms,psparms_data)
CALL ancil_info_assoc(ainfo, ainfo_data)
CALL prognostics_assoc(progs,progs_data)
CALL coastal_assoc(coast,coastal_data)
CALL fluxes_assoc(fluxes,fluxes_data)
CALL rivers_assoc(rivers,rivers_data)
CALL wtrac_jls_assoc(wtrac_jls,wtrac_jls_data)

! Initialise the model ancils
CALL init_ancillaries(nml_dir, rivers, rivers_data)

! Driving data obtained via coupling or via ancillary files
IF (l_oasis_rivers) THEN
  ! Once the river grid is set, define the OASIS partition,  &
  ! write grid, and declare oasis coupled variables
  CALL oasis_partition()
  CALL oasis_grid()
  CALL oasis_variables()
ELSE
  ! Contains allocation of progs_data%seed_rain - hence passing in the data type
  CALL init_drive(nml_dir,ainfo,progs_data)
END IF

! Initialise the model prognostics
CALL init_ic(nml_dir, rivers)

! Initialise output
CALL init_output(nml_dir)

!-----------------------------------------------------------------------------
! Other initialisation that does not depend on further user input.
!-----------------------------------------------------------------------------

! Check that the enabled schemes are compatible
CALL check_compatible_options()

! Seek the input files to the start of the run
CALL seek_all_to_current_datetime()

! Write an initial dump
CALL write_dump()

CALL log_info("init", "Initialisation is complete")

RETURN

END SUBROUTINE init
END MODULE init_mod
