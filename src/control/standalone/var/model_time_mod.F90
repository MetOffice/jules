#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE model_time_mod

USE datetime_mod, ONLY: datetime

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
TYPE(datetime) :: run_min_time, run_max_time
  ! The start and end time for the run
    ! run_min_time is earliest of
    ! main_run_start and spinup_start
    ! run_max_time is latest of
    ! main_run_end and spinup_end

TYPE(datetime) :: current_time  ! The time that the current timestep started

TYPE(datetime) :: main_run_start, main_run_end  ! The start and end times
                                                ! for the main model run

INTEGER :: timestep_number     ! The current timestep number.
INTEGER :: timestep_len        ! The length of the model timestep (s).
INTEGER :: timesteps_in_day    ! The number of timesteps in a day
INTEGER :: oasis_time          ! Time controlling oasis coupling:
                               !  the current timestep in seconds
                               ! This time is not applicable to JULES or
                               ! Rivers-only when not coupled to OASIS and
                               ! should therefore not be used elsewhere.

INTEGER :: print_step = 1  ! Period (number of timesteps) for printing
                           ! of time information to screen.

LOGICAL :: is_spinup  ! Indicates whether the model is in the spinup stage
TYPE(datetime) :: spinup_start, spinup_end  ! The start and end times for
                                            ! each spinup cycle
INTEGER :: max_spinup_cycles = 0  !The maximum number of spinup cycles allowed
INTEGER :: spinup_cycle  ! The cycle of spinup that the model is in
LOGICAL :: terminate_on_spinup_fail = .FALSE.
                                     ! T - terminate run if model fails to
                                     !     fully spin up
                                     ! F - continue with main run regardless

LOGICAL :: end_of_run = .FALSE.  ! Indicates that the run has ended

LOGICAL :: start_of_year = .FALSE.  ! Variables indicating whether the
LOGICAL :: end_of_year = .FALSE.    ! current timestep is the first or last
                                    ! timestep in a year

END MODULE model_time_mod
#endif
