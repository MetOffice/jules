#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ts_enddef(FILE)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes the file out of definition mode - no more dimensions or variables
!   may be defined after this
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ts), INTENT(IN OUT) :: FILE


!-----------------------------------------------------------------------------

FILE%define_mode = .FALSE.


! Verify that a time dimension has been defined - this is necessary for a
! timeseries file
IF ( .NOT. FILE%has_time_dim )                                                 &
  CALL log_fatal("file_ts_enddef",                                             &
                 "Time dimension has not been defined")

! Seek the file so that the first timestep will be read/written when requested
CALL file_ts_seek_to_datetime(FILE, FILE%data_start)

RETURN

END SUBROUTINE file_ts_enddef
#endif
