#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


PROGRAM jules

!$ USE omp_lib, ONLY: omp_get_max_threads

USE init_mod, ONLY: init

USE io_constants, ONLY: max_file_name_len
USE jules_vars_mod, ONLY: mpi_local_comm
USE mpi, ONLY: mpi_comm_world

USE jules_final_mod, ONLY:                                                     &
!  imported procedures
    jules_final

USE next_time_mod, ONLY: next_time

USE time_varying_input_mod, ONLY:                                              &
  update_prescribed_variables => update_model_variables,                       &
  input_close_all => close_all

USE update_derived_variables_mod, ONLY: update_derived_variables

USE output_mod, ONLY: output_initial_data, sample_data, output_data,           &
                      output_close_all => close_all

USE model_time_mod, ONLY: timestep_number, start_of_year, end_of_year,         &
                          end_of_run

USE update_mod, ONLY: l_imogen

USE jules_print_mgr, ONLY: jules_message, jules_print

USE jules_forcing_mod, ONLY: u_1_ij, v_1_ij
USE gridmean_fluxes, ONLY:                                                     &
  fqw_1_ij, ftl_1_ij, taux_1_ij, tauy_1_ij

!TYPE definitions
USE jules_fields_mod, ONLY: crop_vars_data, crop_vars,                         &
                            psparms_data, psparms,                             &
                            top_pdm_data, toppdm,                              &
                            fire_vars_data, fire_vars,                         &
                            ainfo_data, ainfo,                                 &
                            fire_vars_data, fire_vars,                         &
                            trif_vars_data, trif_vars,                         &
                            soil_ecosse_vars_data, soilecosse,                 &
                            aero_data, aerotype,                               &
                            urban_param_data, urban_param,                     &
                            progs_data, progs,                                 &
                            top_pdm_data, toppdm,                              &
                            trifctl_data, trifctltype,                         &
                            coastal_data, coast,                               &
                            jules_vars_data, jules_vars,                       &
                            fluxes_data, fluxes,                               &
                            forcing_data, forcing,                             &
                            lake_data, lake_vars,                              &
                            chemvars_data, chemvars,                           &
                            rivers_data, rivers,                               &
                            water_resources_data, water_resources,             &
                            wtrac_jls_data, wtrac_jls

! In general CABLE utilizes a required subset of tbe JULES types, however;
! CABLE requires extra prognostics and some vars to be kept thru a timestep
USE cable_fields_mod, ONLY: progs_cbl_vars_data, progs_cbl_vars,               &
                            work_vars_data_cbl, work_vars_cbl
USE imgn_drive_mod, ONLY: imgn_drive_data, imgn_drive
USE imgn_vars_mod, ONLY: imgn_vars_data, imgn_vars

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is the main program routine for JULES
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
CHARACTER(LEN=max_file_name_len) :: nml_dir  ! Directory containing namelists

INTEGER :: ERROR  ! Error indicator


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise the MPI environment
!-----------------------------------------------------------------------------
! We don't check the error since most (all?) MPI implementations will just
! fail if a call is unsuccessful
CALL mpi_init(ERROR)
mpi_local_comm = mpi_comm_world

!-----------------------------------------------------------------------------
! If OpenMP is in use provide an information message to make sure the
! user is aware.
!-----------------------------------------------------------------------------
!$ WRITE(jules_message, '(A, I3, A)') 'Using OpenMP with up to ',              &
!$                                        OMP_get_max_threads(), ' thread(s)'
!$ CALL jules_print('jules', jules_message)

!-----------------------------------------------------------------------------
! Try to read a single argument from the command line
!
! If present, that single argument will be the directory we try to read
! namelists from
! If not present, we use current working directory instead
!-----------------------------------------------------------------------------
CALL GET_COMMAND_ARGUMENT(1, nml_dir)
! If no argument is given, GET_COMMAND_ARGUMENT returns a blank string
IF ( LEN_TRIM(nml_dir) == 0 ) nml_dir = "."

!-----------------------------------------------------------------------------
! Initialise the model
!-----------------------------------------------------------------------------
CALL init(nml_dir, crop_vars_data, crop_vars,                                  &
                   psparms_data, psparms,                                      &
                   toppdm, top_pdm_data,                                       &
                   fire_vars, fire_vars_data,                                  &
                   ainfo, ainfo_data,                                          &
                   trif_vars, trif_vars_data,                                  &
                   soilecosse, soil_ecosse_vars_data,                          &
                   aero_data, aerotype,                                        &
                   urban_param, urban_param_data,                              &
                   progs, progs_data,                                          &
                   trifctl_data, trifctltype,                                  &
                   coastal_data, coast,                                        &
                   jules_vars_data, jules_vars,                                &
                   fluxes_data, fluxes,                                        &
                   lake_data, lake_vars,                                       &
                   forcing_data, forcing,                                      &
                   imgn_drive_data, imgn_drive,                                &
                   imgn_vars_data, imgn_vars,                                  &
                  !veg3_parm_(data), &
                   !veg3_field_(data), &
                   chemvars_data, chemvars,                                    &
                   rivers_data, rivers,                                        &
                   water_resources_data, water_resources,                      &
                   wtrac_jls_data, wtrac_jls,                                  &
                   ! CABLE state vars, progs, params and miscellaneous
                   ! requirements
                   progs_cbl_vars_data, progs_cbl_vars,                        &
                   work_vars_data_cbl, work_vars_cbl                           &
                   )

!-----------------------------------------------------------------------------
! Loop over timesteps.
! Note that the number of timesteps is of unknown length at the start of run,
! if the model is to determine when it has spun up.
!-----------------------------------------------------------------------------
DO    !  timestep

  !-----------------------------------------------------------------------------
  ! Update the IMOGEN climate variables if required
  !-----------------------------------------------------------------------------
  IF ( l_imogen .AND. start_of_year ) THEN
    CALL update_prescribed_variables()   ! read in required variables
    CALL imogen_update_clim(progs, imgn_drive, imgn_vars, ainfo)
  END IF

  !-----------------------------------------------------------------------------
  ! The update of prescribed data is done in two phases
  !  - Update variables provided by files
  !  - Update variables that are derived from those updated in the first phase
  !-----------------------------------------------------------------------------
  IF ( .NOT. l_imogen) THEN
    CALL update_prescribed_variables()
  END IF
  CALL update_derived_variables(crop_vars,psparms,ainfo,urban_param,progs,     &
                                jules_vars, forcing, imgn_drive)

  !-----------------------------------------------------------------------------
  ! Check if this is a timestep that we need to output initial data for (i.e.
  ! start of spinup cycle or start of main run), and output that data if
  ! required
  !-----------------------------------------------------------------------------
  CALL output_initial_data()

  !-----------------------------------------------------------------------------
  ! Call the main model science routine
  !-----------------------------------------------------------------------------
  CALL control(                                                                &
  !   Scalar arguments (INTENT IN)
      timestep_number,                                                         &
  !   Forcing (INTENT IN)
      u_1_ij, v_1_ij,                                                          &
  !   Gridbox mean surface fluxes (INTENT OUT)
      fqw_1_ij, ftl_1_ij, taux_1_ij, tauy_1_ij,                                &
      !TYPES containing field data (IN OUT)
      crop_vars,psparms,toppdm,fire_vars,ainfo,trif_vars,soilecosse, aerotype, &
      urban_param,progs,trifctltype, coast, jules_vars,                        &
      fluxes,                                                                  &
      lake_vars,                                                               &
      forcing,                                                                 &
      !veg3_parm, &
      !veg3_field, &
      chemvars,                                                                &
      rivers, water_resources,                                                 &
      wtrac_jls,                                                               &
      progs_cbl_vars,                                                          &
      work_vars_cbl                                                            &
      )

  !-----------------------------------------------------------------------------
  ! Update IMOGEN carbon if required
  !-----------------------------------------------------------------------------
  IF ( l_imogen .AND. end_of_year ) CALL imogen_update_carb(progs, imgn_vars)

  !-----------------------------------------------------------------------------
  ! Sample variables for output
  !-----------------------------------------------------------------------------
  CALL sample_data()

  !-----------------------------------------------------------------------------
  ! Output collected data if required
  !-----------------------------------------------------------------------------
  CALL output_data()

  !-----------------------------------------------------------------------------
  ! Move the model on to the next timestep
  !-----------------------------------------------------------------------------
  CALL next_time(progs, trifctltype)

  IF ( end_of_run ) EXIT

END DO  !  timestep loop

!-----------------------------------------------------------------------------
! Clean up by closing all open files
!-----------------------------------------------------------------------------
CALL input_close_all()
CALL output_close_all()

!-----------------------------------------------------------------------------
! Final messages.
!-----------------------------------------------------------------------------
CALL jules_final()

!-----------------------------------------------------------------------------
! Clean up the MPI environment
!-----------------------------------------------------------------------------
CALL mpi_finalize(ERROR)


END PROGRAM jules
#endif
